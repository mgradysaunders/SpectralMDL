// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <optional>

#if defined(_WIN32)
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h> // for 'GetProcessTimes'
#endif               // #if defined(_WIN32)

#include "assimp/version.h"
#include "embree4/rtcore_config.h"
#include "opensubdiv/version.h"

#include "CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/WithColor.h"

#include "Color.h"
#include "Layout/Layout.h"
#include "Layout/LayoutTables.h"
#include "Progress.h"
#include "Render/Autolook.h"
#include "Render/Camera.h"
#include "Render/Guiding.h"
#include "Render/Light.h"
#include "Render/Manifold.h"
#include "Render/PathTracing.h"
#include "Render/Sampler.h"
#include "Scene/Scene.h"
#include "Tonemap.h"

#include "smdl/Common.h"
#include "smdl/RenderUtil/SpectralFilm.h"
#include "smdl/Support/Denormals.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Profiler.h"

// Optional rather than required only because the '.places' utility
// flags below run without a scene; everything else checks it by hand
// right after they bow out.
static cl::opt<std::string> optInputSceneFile{cl::Positional,
                                              cl::desc("<input scene>")};
static cl::list<std::string> optInputMDLFiles{
    cl::Positional, cl::desc("<input mdl>"), cl::ZeroOrMore};
//--{ CLI: Scene Options
static cl::OptionCategory catScene{"Scene Options"};
static cl::opt<float> optTime{
    "time",
    cl::desc("The animation time in seconds at shutter open, overriding the "
             "layout's 'time' directive (default: 0)"),
    cl::init(0.0f), cl::cat(catScene)};
static cl::list<std::string> optInputMeshFiles{
    "mesh", cl::desc("Add another mesh, repeatable"), cl::cat(catScene)};
static cl::list<std::string> optAssetDirs{
    "asset-dir",
    cl::desc("Add a directory to search for assets and meshes, repeatable"),
    cl::cat(catScene)};
static cl::opt<bool> optAllMaterials{
    "all-materials",
    cl::desc("Compile every material in the given MDL modules unconditionally"),
    cl::init(false), cl::cat(catScene)};
static cl::opt<bool> optGround{"ground",
                               cl::desc("Add a ground plane under the scene"),
                               cl::init(false), cl::cat(catScene)};
static cl::opt<float> optGroundZ{
    "ground-z",
    cl::desc("Place the ground plane at this height (implies -ground)"),
    cl::init(0.0f), cl::cat(catScene)};
static cl::opt<std::string> optGroundMaterial{
    "ground-material",
    cl::desc("With -ground, the MDL material for the ground plane (default: 10 "
             "percent gray)"),
    cl::cat(catScene)};
static cl::opt<std::string> optFallbackMaterial{
    "fallback-material",
    cl::desc("The MDL material for names the scene does not resolve "
             "(default: none, an error)\n"
             "* 'default_object' is built in, a plain 20 percent Lambertian"),
    cl::cat(catScene)};
//--}
//--{ CLI: Utility Options
static cl::OptionCategory catUtility{"Utility Options"};
static cl::opt<std::string> optDumpPlaces{
    "dump-places",
    cl::desc("Print '.places' buffer as one-line place text, then exit"),
    cl::cat(catUtility)};
static cl::opt<std::string> optDumpCurves{
    "dump-curves", cl::desc("Print '.curves' file summary, then exit"),
    cl::cat(catUtility)};
static cl::opt<std::string> optPackPlaces{
    "pack-places",
    cl::desc("Pack layout's 'place' statements into a '.places' buffer, "
             "then exit"),
    cl::cat(catUtility)};
static cl::opt<std::string> optOutputPlaces{
    "pack-places-file",
    cl::desc(
        "The output file for -pack-places (default: layout name + '.places')"),
    cl::cat(catUtility)};
static cl::opt<bool> optListMaterials{
    "list-materials", cl::desc("List material names the scene needs and exit"),
    cl::init(false), cl::cat(catUtility)};
static cl::opt<bool> optListObjects{
    "list-objects",
    cl::desc("List objects present in each scene file and exit"),
    cl::init(false), cl::cat(catUtility)};
static cl::opt<bool> optJSON{
    "json",
    cl::desc(
        "With -list-objects or -list-materials, print JSON instead of a table"),
    cl::init(false), cl::cat(catUtility)};
//--}
//--{ CLI: Sampling Options
static cl::OptionCategory catSampling{"Sampling Options"};
static cl::opt<unsigned> optSPP{
    "spp", cl::desc("The number of samples per pixel (default: 8)"),
    cl::init(8U), cl::cat(catSampling)};
static cl::opt<unsigned> optSampleOffset{
    "sample-offset",
    cl::desc("The sample index this render starts from, to decorrelate renders "
             "(default: 0, -resume overrides)"),
    cl::init(0), cl::cat(catSampling)};
static cl::opt<unsigned> optMaxBounces{
    "max-bounces",
    cl::desc("Trace every path to at most this many bounces with no Russian "
             "roulette (default: roulette, backstopped at 63)\n"
             "* a bounce is a scattering event: 0 keeps only the emission the "
             "camera sees directly, 1 adds direct lighting"),
    cl::init(63U), cl::cat(catSampling)};
static cl::opt<float> optMaxContribution{
    "max-contribution",
    cl::desc("Limit any single contribution to this per-band radiance "
             "(default: 0, off)"),
    cl::init(0.0f), cl::cat(catSampling)};
static cl::opt<unsigned> optMaxContributionBounces{
    "max-contribution-bounces",
    cl::desc("With -max-contribution, only bound contributions of at least "
             "this many bounces (default: 1)"),
    cl::init(1U), cl::cat(catSampling)};
static cl::opt<bool> optNoLOD{
    "no-lod", cl::desc("Disable LOD by zeroing the camera ray cone spread"),
    cl::init(false), cl::cat(catSampling)};
static cl::opt<unsigned> optThreads{
    "threads",
    cl::desc("Set the thread limit, or 0 for the maximum (default: 0)\n"
             "* '-threads 1' runs inline with no pool at all, for a debugger"),
    cl::init(0), cl::cat(catSampling)};
static cl::opt<bool> optGuide{"guide", cl::desc("Enable SD-tree path guiding"),
                              cl::init(false), cl::cat(catSampling)};
static cl::opt<bool> optGuideADRRS{
    "guide-adrrs",
    cl::desc("With -guide, drive Russian roulette by expected pixel "
             "contribution instead of throughput (default: true)\n"
             "* moot with -max-bounces, which turns roulette off"),
    cl::init(true), cl::cat(catSampling)};
static cl::opt<float> optGuideBSDFFraction{
    "guide-bsdf-fraction",
    cl::desc("With -guide, probability of sampling the BSDF instead of "
             "the SD-tree at guided vertices (default: 0.5)"),
    cl::init(0.5f), cl::cat(catSampling)};
static cl::opt<float> optGuideSplit{
    "guide-split",
    cl::desc("With -guide, SD-tree spatial split threshold in records "
             "(default: 12000)"),
    cl::init(12000.0f), cl::cat(catSampling)};
static cl::opt<bool> optMNEE{"mnee",
                             cl::desc("Enable manifold next-event estimation"),
                             cl::init(false), cl::cat(catSampling)};
static cl::opt<unsigned> optMNEEDepth{
    "mnee-depth",
    cl::desc("With -mnee, maximum number of refractive interfaces a "
             "connection may cross, 1 to 4 (default: 4)"),
    cl::init(4), cl::cat(catSampling)};
static cl::opt<unsigned> optMNEEMaxTrials{
    "mnee-max-trials",
    cl::desc("With -mnee, max attempts to re-find a reciprocal "
             "estimate before dropping the sample (default: 256)"),
    cl::init(256), cl::cat(catSampling)};
static cl::opt<float> optMNEEReceiverAlpha{
    "mnee-receiver-alpha",
    cl::desc("With -mnee, squared roughness needed to be a "
             "receiver (default: 0.005, 0 takes every finite lobe)"),
    cl::init(0.005f), cl::cat(catSampling)};
static cl::opt<unsigned> optMNEEBiased{
    "mnee-biased",
    cl::desc("With -mnee, enable biased mode with this many walks per estimate "
             "(default: 0, unbiased)"),
    cl::init(0), cl::cat(catSampling)};
static cl::opt<float> optMNEEMaxRoughness{
    "mnee-max-roughness",
    cl::desc("With -mnee, do not claim glossy lobes with roughness wider than "
             "this (default: 0, no limit)"),
    cl::init(0.0f), cl::cat(catSampling)};
static cl::opt<bool> optMNEESunOnly{
    "mnee-sun-only",
    cl::desc("With -mnee and the procedural sun-sky, restrict the Dirac-chain "
             "machinery to the sun disk"),
    cl::init(false), cl::cat(catSampling)};
static cl::opt<bool> optMNEEReport{
    "mnee-report",
    cl::desc("With -mnee, print the manifold estimator stats after the render"),
    cl::init(false), cl::cat(catSampling)};
static cl::opt<bool> optMNEETestNormalHook{
    "mnee-test-normalhook",
    cl::desc("Test the geometry-normal hook against the meshes and exit, "
             "non-zero on failure"),
    cl::init(false), cl::cat(catSampling)};
//--}
//--{ CLI: Camera Options
static cl::OptionCategory catCamera{"Camera Options"};
static cl::opt<int2> optResolution{
    "resolution",
    cl::desc("The image dimensions in pixels (default: 1280,720)"),
    cl::init(int2{1280, 720}), cl::cat(catCamera)};
static cl::opt<int4> optCropWindow{
    "crop-window",
    cl::desc("Render only pixels x0 <= x < x1, y0 <= y < y1 of the -resolution "
             "frame, given as x0,y0,x1,y1 (default: the whole frame)\n"
             "* the output keeps the full size with the rest black"),
    cl::init(int4{0, 0, 0, 0}), cl::cat(catCamera)};
static cl::opt<float3> optLookFrom{
    "look-from", cl::desc("The position to look from (default: -6,0,2)"),
    cl::init(float3{-6, 0, 2}), cl::cat(catCamera)};
static cl::opt<float3> optLookTo{
    "look-to", cl::desc("The position to look to (default: 0,0,0.5)"),
    cl::init(float3{0, 0, 0.5}), cl::cat(catCamera)};
static cl::opt<float3> optLookUp{"look-up",
                                 cl::desc("The up vector (default: 0,0,1)"),
                                 cl::init(float3{0, 0, 1}), cl::cat(catCamera)};
static cl::opt<float> optFOV{
    "fovy", cl::desc("The vertical FOV in degrees (default: 37.8)"),
    cl::init(37.8f), cl::cat(catCamera)};
static cl::opt<std::string> optWavelengthRange{
    "wavelength-range",
    cl::desc("Uniform wavelengths spanning A to B nm with N bands, "
             "format 'A,B:N' where ':N' is optional (default: 380,720:16)"),
    cl::cat(catCamera)};
static cl::opt<std::string> optWavelengths{
    "wavelengths",
    cl::desc("Explicit wavelengths in nm, comma-separated or a text file of "
             "whitespace-separated values (mutually exclusive with "
             "-wavelength-range)"),
    cl::cat(catCamera)};
static cl::opt<bool> optWavelengthJitter{
    "wavelength-jitter",
    cl::desc("Jitter each wavelength within its band, so that every band "
             "estimates the mean radiance over the band rather than the "
             "radiance at one wavelength\n"
             "* the outermost bands reach half a band past the grid ends"),
    cl::init(false), cl::cat(catCamera)};
static cl::opt<bool> optAutolook{
    "autolook",
    cl::desc("Solve -look-from/-look-to to fit the scene at the given FOV"),
    cl::init(false), cl::cat(catCamera)};
// Kept apart from -autolook on purpose: folding the azimuth into an
// optional value of -autolook would make LLVM demand '-autolook=N' (a
// space never binds the value) and hide a bare -autolook from
// -print-options.
static cl::opt<float> optAutolookAzimuth{
    "autolook-azimuth",
    cl::desc("With -autolook, the azimuth of the scene-to-camera direction in "
             "degrees CCW from +X (default: solved for frame fill)"),
    cl::init(0.0f), cl::cat(catCamera)};
static cl::opt<float> optAutolookZenith{
    "autolook-zenith",
    cl::desc("With -autolook, the zenith angle of the scene-to-camera "
             "direction in degrees (default: 65, the standard 3/4 view)"),
    cl::init(65.0f), cl::cat(catCamera)};
static cl::opt<float> optAutolookMargin{
    "autolook-margin",
    cl::desc("With -autolook, the padding to the frame edge as a fraction of "
             "the frame (default: 0.05)"),
    cl::init(0.05f), cl::cat(catCamera)};
static cl::opt<bool> optAutolookIgnoreBackfaces{
    "autolook-ignore-backfaces",
    cl::desc("With -autolook, neither avoid nor warn about views of backfacing "
             "geometry"),
    cl::init(false), cl::cat(catCamera)};
//--}
//--{ CLI: Camera-Lens Options
static cl::OptionCategory catCameraLens{"Camera-Lens Options"};
static cl::opt<float> optShutterSpeed{
    "shutter-speed",
    cl::desc("The seconds the shutter stays open, overriding the layout's "
             "'time' directive (default: 0, shut)"),
    cl::init(0.0f), cl::cat(catCameraLens)};
static cl::opt<float> optFStop{
    "fstop", cl::desc("Enable DOF by f-number assuming 35mm-format frame"),
    cl::init(0.0f), cl::cat(catCameraLens)};
static cl::opt<float> optAperture{
    "aperture",
    cl::desc("Enable DOF by aperture radius in scene units (mutually exclusive "
             "with -fstop)"),
    cl::init(0.0f), cl::cat(catCameraLens)};
static cl::opt<float> optFocus{
    "focus",
    cl::desc("The focus distance along the view axis in scene units (default: "
             "distance between -look-from and -look-to)"),
    cl::init(0.0f), cl::cat(catCameraLens)};
static cl::opt<int> optBlades{
    "blades",
    cl::desc("The number of aperture blades (default: 0, a round lens)"),
    cl::init(0), cl::cat(catCameraLens)};
static cl::opt<float> optBladeAngle{
    "blade-angle",
    cl::desc("With -blades, the rotation of the aperture polygon in "
             "degrees (default: 0, vertex at screen right)"),
    cl::init(0.0f), cl::cat(catCameraLens)};
static cl::opt<float> optDistortionK1{
    "distortion-k1",
    cl::desc("The radial distortion, in relative corner displacement (barrel "
             ">0, pincushion <0, default: 0)"),
    cl::init(0.0f), cl::cat(catCameraLens)};
static cl::opt<float> optDistortionK2{
    "distortion-k2",
    cl::desc("The quartic term of radial distortion, same units as "
             "-distortion-k1 (default: 0)"),
    cl::init(0.0f), cl::cat(catCameraLens)};
static cl::opt<bool> optDistortionFit{
    "distortion-fit",
    cl::desc("Refit so frame corner directions hold constant under distortion"),
    cl::init(false), cl::cat(catCameraLens)};
static cl::opt<float> optVignetting{
    "vignetting",
    cl::desc("The strength of cos^4 falloff (default: 0 is off, 1 is the "
             "physical law)"),
    cl::init(0.0f), cl::cat(catCameraLens)};
static cl::opt<float> optCatEye{
    "cat-eye",
    cl::desc(
        "With -fstop or -aperture, mechanical vignette from the lens barrel\n"
        "* corner displacement in rim radii (0 is off, 1 is fully dark)"),
    cl::init(0.0f), cl::cat(catCameraLens)};
static cl::opt<float> optCatEyeRadius{
    "cat-eye-radius",
    cl::desc("With -cat-eye, the barrel rim radius in scene units (default: "
             "the aperture radius, i.e., wide-open)"),
    cl::init(0.0f), cl::cat(catCameraLens)};
//--}
//--{ CLI: Light Options
static cl::OptionCategory catLight{"Light Options"};
static cl::opt<bool> optNoSunSky{
    "no-sky",
    cl::desc("Disable the default sun-sky, restoring the black "
             "environment"),
    cl::init(false), cl::cat(catLight)};
static cl::opt<float> optSunZenith{
    "sun-zenith",
    cl::desc("The solar zenith angle in degrees, 5-88 (default: 42)"),
    cl::init(42.0f), cl::cat(catLight)};
static cl::opt<float> optSunAzimuth{
    "sun-azimuth",
    cl::desc("The solar azimuth angle in degrees CCW from +X (default: 135)"),
    cl::init(135.0f), cl::cat(catLight)};
static cl::opt<float> optSkyVisibility{
    "visibility", cl::desc("The aerosol visibility in km, 5-100 (default: 23)"),
    cl::init(23.0f), cl::cat(catLight)};
static cl::opt<float> optSkyWaterVapor{
    "water-vapor",
    cl::desc("The water-vapor column scale factor, 0.3-3 (default: 1)"),
    cl::init(1.0f), cl::cat(catLight)};
static cl::opt<bool> optHaze{
    "haze",
    cl::desc("Enable the exterior haze that produces aerial perspective, "
             "which a layout's 'haze' block configures"),
    cl::init(false), cl::cat(catLight)};
static cl::opt<bool> optNoHaze{
    "no-haze", cl::desc("Disable the exterior haze a layout asked for"),
    cl::init(false), cl::cat(catLight)};
static cl::opt<float> optHazeVisibility{
    "haze-visibility",
    cl::desc("The haze meteorological range in km at 550nm (default: the "
             "sky's visibility)"),
    cl::init(0.0f), cl::cat(catLight)};
static cl::opt<float> optHazeScaleHeight{
    "haze-scale-height",
    cl::desc("The haze scale height in meters (default: 2100)"),
    cl::init(2100.0f), cl::cat(catLight)};
static cl::opt<float> optSkyScale{
    "sky-scale", cl::desc("The sky radiance scale factor (default: 1)"),
    cl::init(1.0f), cl::cat(catLight)};
static cl::opt<float> optMoonPhase{
    "moon",
    cl::desc("Enable moonlight mode at this signed phase angle in degrees\n"
             "* 0 is full, +/-180 is new, the sign picks waxing or waning\n"
             "* radiance is ~1e-6 of daylight, use with '-tonemap night'"),
    cl::init(0.0f), cl::cat(catLight)};
static cl::opt<float> optMoonDistance{
    "moon-distance",
    cl::desc("With -moon, the lunar distance factor (default: 1, realistic "
             "range ~0.86-1.14)"),
    cl::init(1.0f), cl::cat(catLight)};
static cl::opt<std::string> optIBLFilename{
    "ibl",
    cl::desc("The IBL filename (any supported format, likely '.hdr', '.exr')"),
    cl::cat(catLight)};
static cl::opt<float> optIBLScale{
    "ibl-scale", cl::desc("With -ibl, the IBL scale factor (default: 1)"),
    cl::init(1.0f), cl::cat(catLight)};
static cl::opt<bool> optAllLights{
    "all-lights",
    cl::desc("Aim light selection at every emissive surface, marked 'light' "
             "in the layout or not (a scene given without a layout marks "
             "everything already)"),
    cl::init(false), cl::cat(catLight)};
static cl::opt<bool> optNoLightTree{
    "no-light-tree",
    cl::desc("Select lights from the flat power-weighted distribution, the "
             "same at every receiver, instead of the light tree, which "
             "weighs each light by its power over its squared distance to "
             "the receiver"),
    cl::init(false), cl::cat(catLight)};
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
* 'night' models human vision at absolute luminance and auto-exposes,
  for physically dim scenes like moonlight)"),
    cl::init(std::string("linear")), cl::cat(catTonemap)};
static cl::opt<float> optTonemapDecades{
    "tonemap-decades",
    cl::desc("With -curve=log, how many decades below white reach "
             "black (default: 4)"),
    cl::init(4.0f), cl::cat(catTonemap)};
static cl::opt<std::string> optCurve{
    "curve", cl::desc(R"(The display curve for 8-bit output (default: gamma)
* 'gamma' clamps and gamma-encodes
* 'log' maps decades below the exposure-scaled white point
* 'filmic' rolls highlights off toward white instead of clipping them)"),
    cl::init(std::string("gamma")), cl::cat(catTonemap)};
static cl::opt<std::string> optLocal{
    "local", cl::desc(R"(Local tone mapping for 8-bit output (default: off)
* 'fusion' by Laplacian pyramid; also auto-exposes, leaving -exposure a relative adjustment)"),
    cl::init(std::string("off")), cl::cat(catTonemap)};
static cl::opt<float> optLocalStrength{
    "local-strength",
    cl::desc(
        "With -local, how much local exposure to keep, 0 to 1 (default: 0.75)"),
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
    cl::desc("Force false color band mapping for the RGB outputs\n"
             "* engages automatically when wavelength grid does not cover the "
             "visible"),
    cl::init(false), cl::cat(catTonemap)};
static cl::opt<float3> optRGBWaves{
    "rgb-wavelengths",
    cl::desc("With -false-color, the wavelengths in nm mapped to R,G,B "
             "(default: 5/6, 1/2, and 1/6 of the grid span, long to red)"),
    cl::init(float3{}), cl::cat(catTonemap)};
//--}
//--{ CLI: Output Options
static cl::OptionCategory catOutput{"Output Options"};
static cl::opt<std::string> optOutputRGB{
    "output-rgb",
    cl::desc("The tone mapped RGB image filename (default: output.png)"),
    cl::init(std::string("output.png")), cl::cat(catOutput)};
static cl::opt<std::string> optOutputRGBf{
    "output-rgbf",
    cl::desc("Also write linear RGB radiance to this '.exr' or '.hdr' file"),
    cl::cat(catOutput)};
static cl::opt<std::string> optOutputSpectrum{
    "output-spectrum",
    cl::desc("Also write linear spectral radiance to this ENVI file"),
    cl::cat(catOutput)};
static cl::opt<std::string> optResume{
    "resume",
    cl::desc("Resume accumulating from this ENVI file written by a previous "
             "-output-spectrum"),
    cl::cat(catOutput)};
static cl::opt<std::string> optProgressFile{
    "progress-file",
    cl::desc("Write 'done=N total=M elapsed=S eta=S note=...' progress into "
             "this file, about ten times a second"),
    cl::cat(catOutput)};
static cl::opt<double> optPreviewEvery{
    "preview-every",
    cl::desc("Rewrite '-output-rgb' about this often in seconds "
             "(default: 0, off)"),
    cl::init(0.0), cl::cat(catOutput)};
static cl::opt<std::string> optProgress{
    "progress",
    cl::desc("Draw a progress bar while rendering: 'auto', 'plain', or 'none' "
             "(default: auto)"),
    cl::init(std::string("auto")), cl::cat(catOutput)};
static cl::opt<std::string> optProfile{
    "profile",
    cl::desc("Write a time-trace JSON of everything before rendering starts "
             "(default: smdl-toy.trace.json)\n"
             "* open in chrome://tracing or https://ui.perfetto.dev"),
    cl::ValueOptional, cl::init(std::string{}), cl::cat(catOutput)};
//--}

// The command line, joined for the `smdl args` metadata field, with the
// session-only flags stripped: outputs, display transforms, the sample
// budget, the guiding strategy, the thread count, and -resume itself
// legitimately change between the sessions of one render, while anything
// else that differs likely changes the radiance being estimated and
// earns a warning. The wavelength and window flags are stripped too: a
// genuine grid or window mismatch already has its own hard error, so
// warning here would double-report. Tokenizes on whitespace, so a path
// containing spaces can misalign the comparison; the result only feeds a
// warning, never behavior.
[[nodiscard]]
static std::vector<std::string> stripSessionOnlyArgs(const std::string &args) {
  // Split by whether the flag's value arrives as a separate token, so
  // that token is stripped with it; the boolean guiding flags carry no
  // value and must not eat the token after them.
  static constexpr auto SESSION_ONLY_VALUES = std::array{"resume",
                                                         "spp",
                                                         "output-rgb",
                                                         "output-rgbf",
                                                         "output-spectrum",
                                                         "exposure",
                                                         "tonemap",
                                                         "tonemap-decades",
                                                         "curve",
                                                         "local",
                                                         "local-strength",
                                                         "local-range",
                                                         "local-clamp",
                                                         "wavelength-range",
                                                         "wavelengths",
                                                         "crop-window",
                                                         "guide-bsdf-fraction",
                                                         "guide-split",
                                                         "mnee-depth",
                                                         "mnee-max-trials",
                                                         "mnee-biased",
                                                         "mnee-max-roughness",
                                                         "mnee-receiver-alpha",
                                                         "sample-offset",
                                                         "threads"};
  static constexpr auto SESSION_ONLY_FLAGS =
      std::array{"guide",         "guide-adrrs", "mnee",
                 "mnee-sun-only", "mnee-report", "mnee-test-normalhook"};
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
      for (const auto *sessionOnlyName : SESSION_ONLY_VALUES)
        if (name == sessionOnlyName) {
          isSessionOnly = true;
          takesValue = true;
          break;
        }
      if (!isSessionOnly)
        for (const auto *sessionOnlyName : SESSION_ONLY_FLAGS)
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
[[nodiscard]]
static std::vector<float> parseWavelengthsFlag(const std::string &flagValue) {
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

struct WavelengthRange final {
  float2 range{};
  unsigned bandCount{};
};

// Parse the '-wavelength-range' flag: 'A,B:N' for N uniform bands
// spanning A to B nm, with ':N' optional. Returns the default grid when
// the flag was not given.
[[nodiscard]]
static WavelengthRange parseWavelengthRangeFlag(const std::string &flagValue) {
  auto result{WavelengthRange{float2{WAVELENGTH_MIN, WAVELENGTH_MAX}, 16U}};
  if (flagValue.empty()) return result;
  const char *ptr{flagValue.c_str()};
  char *numEnd{};
  result.range.x = std::strtof(ptr, &numEnd);
  if (numEnd == ptr || *numEnd != ',')
    throw smdl::Error(smdl::concat("cannot parse -wavelength-range near ",
                                   smdl::Quoted(std::string(ptr, 0, 12))));
  ptr = numEnd + 1;
  result.range.y = std::strtof(ptr, &numEnd);
  if (numEnd == ptr)
    throw smdl::Error(smdl::concat("cannot parse -wavelength-range near ",
                                   smdl::Quoted(std::string(ptr, 0, 12))));
  ptr = numEnd;
  if (*ptr == ':') {
    ptr++;
    if (!std::isdigit(static_cast<unsigned char>(*ptr)))
      throw smdl::Error(smdl::concat("cannot parse -wavelength-range near ",
                                     smdl::Quoted(std::string(ptr, 0, 12))));
    result.bandCount = unsigned(std::strtoul(ptr, &numEnd, 10));
    ptr = numEnd;
  }
  if (*ptr != '\0')
    throw smdl::Error(smdl::concat("cannot parse -wavelength-range near ",
                                   smdl::Quoted(std::string(ptr, 0, 12))));
  if (!(std::isfinite(result.range.x) && std::isfinite(result.range.y) &&
        result.range.x > 0 && result.range.x < result.range.y))
    throw smdl::Error(
        "expected -wavelength-range 'A,B' to be positive and increasing");
  if (result.bandCount < 2)
    throw smdl::Error("expected -wavelength-range ':N' to be at least 2");
  return result;
}

// The material a scene falls back to when it has none of its own: a
// plain 20 percent Lambertian, so a layout can be looked at before any
// material has been written for it. Deliberately the dullest thing that
// can be shaded, so nothing about it can be mistaken for a shading
// decision the scene has not made yet.
constexpr const char *DEFAULT_MATERIAL_SOURCE = R"(#smdl
import ::df::*;

export material default_object() = material(
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

// The material `-fallback-material` names to get it.
constexpr const char *DEFAULT_OBJECT_MATERIAL_NAME = "default_object";

// The darker gray `-ground` defaults to, for contrast against the
// default material above.
constexpr const char *DEFAULT_GROUND_MATERIAL_NAME = "default_ground";

// The process CPU time in seconds, summed over every thread, so a render
// on N cores accrues about N seconds per second of wall clock. Zero when
// the platform has no way to ask, which the caller sees as a session that
// took no compute time rather than as an error.
//
// NOTE: 'std::clock()' is only the right answer where nothing better is
// available: it measures process CPU time on POSIX but wall clock since
// process start on MSVC.
[[nodiscard]] static double cpuTimeSeconds() {
#if defined(_WIN32)
  FILETIME creationTime{}, exitTime{}, kernelTime{}, userTime{};
  if (!GetProcessTimes(GetCurrentProcess(), &creationTime, &exitTime,
                       &kernelTime, &userTime))
    return 0.0;
  // Both are 100-nanosecond tick counts in a split 64-bit integer.
  const auto toSeconds{[](const FILETIME &fileTime) {
    return 1e-7 * double((uint64_t(fileTime.dwHighDateTime) << 32) |
                         uint64_t(fileTime.dwLowDateTime));
  }};
  return toSeconds(kernelTime) + toSeconds(userTime);
#elif defined(CLOCK_PROCESS_CPUTIME_ID)
  timespec time{};
  if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time) != 0) return 0.0;
  return double(time.tv_sec) + 1e-9 * double(time.tv_nsec);
#else
  return double(std::clock()) / double(CLOCKS_PER_SEC);
#endif
}

// How the sample budget is split into passes.
//
// Without guiding there is a single pass of the whole budget. With guiding,
// passes grow geometrically (1, 2, 4, ... spp) and the remainder is dumped
// into the final pass, so it always holds at least half the budget. Solved
// up front rather than as the loop runs so that the progress bar can say
// which pass of how many.
[[nodiscard]]
static std::vector<size_t> solveSamplePasses(size_t spp, bool guide,
                                             size_t trainedSpp) {
  // The geometric warmup exists to bound the samples spent while the
  // tree is immature, so a session that resumed a saved tree skips it:
  // the first pass starts at the largest power of two at or below what
  // already trained the tree, and the refine threshold keeps scaling
  // with the pass size.
  size_t firstPass{1};
  while (guide && firstPass * 2 <= trainedSpp) firstPass *= 2;
  auto passes{std::vector<size_t>()};
  for (size_t sppDone{0}; sppDone < spp;) {
    size_t thisPass{guide ? std::min(firstPass << passes.size(), spp - sppDone)
                          : spp};
    if (guide && (spp - sppDone) < 2 * thisPass) thisPass = spp - sppDone;
    passes.push_back(thisPass);
    sppDone += thisPass;
  }
  return passes;
}

// The '-mnee-test-normalhook' pass: at deterministic quasi-random points of
// every surface instance, read the shading normal field through the
// geometry-normal hook (with its central-difference partials) and compare
// against the analytic mesh derivatives. Wherever the material leaves
// 'geometry.normal' alone the two are the same field and must agree; a
// remapped material has no analytic truth to compare against, so it
// reports how far its field bends instead. Returns the number of
// unmapped instances that disagree.
[[nodiscard]] static int runMNEETestNormalHook(const Scene &scene) {
  constexpr int NUM_SAMPLES{64};
  constexpr float TOLERANCE{1e-3f};
  int failures{0};
  for (uint32_t instIndex = 0; instIndex < scene.meshInstances.size();
       instIndex++) {
    const auto &instance{scene.meshInstances[instIndex]};
    if (instance.isCurves()) continue;
    const auto matIndex{scene.materialIndexOf(instance)};
    const auto *material{scene.materials[matIndex]};
    if (!material || !material->geometryNormalEvaluate) continue;
    const size_t faceCount{
        instance.isPrimitive()
            ? size_t(1)
            : scene.meshes[instance.meshIndex]->faces.size()};
    if (faceCount == 0) continue;
    const bool remapped{material->remapsNormal()};
    float maxNormalDot{-1.0f};
    float minNormalDot{+1.0f};
    float maxPartialErr{0.0f};
    int samples{0};
    for (int k = 0; k < NUM_SAMPLES; k++) {
      // Deterministic low-discrepancy-ish points, interior to the face
      // parameterization so the central differences stay inside it.
      const auto faceIndex{uint32_t((size_t(k) * 2654435761UL) % faceCount)};
      const float u{0.05f + 0.35f * std::fmod(0.618034f * float(k + 1), 1.0f)};
      const float v{0.05f + 0.35f * std::fmod(0.754878f * float(k + 2), 1.0f)};
      const auto hit{scene.makeHit(instIndex, faceIndex,
                                   float3(1.0f - u - v, u, v), 0.0f)};
      if (!hit.instance) continue;
      const auto meshGeometry{scene.manifoldGeometry(hit)};
      ManifoldGeometry hookGeometry{};
      if (!manifoldHookGeometry(scene, hit, hookGeometry)) continue;
      samples++;
      const float normalDot{dot(meshGeometry.normal, hookGeometry.normal)};
      maxNormalDot = std::max(maxNormalDot, normalDot);
      minNormalDot = std::min(minNormalDot, normalDot);
      for (int axis = 0; axis < 2; axis++) {
        const float3 &a{axis == 0 ? meshGeometry.dNdu : meshGeometry.dNdv};
        const float3 &b{axis == 0 ? hookGeometry.dNdu : hookGeometry.dNdv};
        const float scale{std::max(length(a), length(b))};
        if (scale > 1e-4f)
          maxPartialErr = std::max(maxPartialErr, length(a - b) / scale);
      }
    }
    if (samples == 0) continue;
    if (remapped) {
      std::cout << "  " << scene.fileNames[instIndex] << " (material "
                << scene.materialNames[matIndex]
                << "): remapped, max bend from mesh normal "
                << smdl::degrees(
                       std::acos(std::clamp(minNormalDot, -1.0f, 1.0f)))
                << " deg over " << samples << " samples\n";
      continue;
    }
    const bool ok{minNormalDot > 1.0f - TOLERANCE &&
                  maxPartialErr <= TOLERANCE};
    if (!ok) failures++;
    std::cout << "  " << scene.fileNames[instIndex] << " (material "
              << scene.materialNames[matIndex]
              << "): " << (ok ? "OK" : "MISMATCH") << ", max relative dN error "
              << maxPartialErr << " over " << samples << " samples\n";
  }
  return failures;
}

int main(int argc, char **argv) try {
  llvm::InitLLVM X(argc, argv);
  // Prints exactly like 'print_to_cerr', except that it knows to step
  // around a progress bar while one is on screen.
  smdl::Logger::get().addSink<ProgressLogSink>();
  cl::SetVersionPrinter([](llvm::raw_ostream &os) {
    auto info{smdl::BuildInfo::get()};
    info.thirdparty.push_back({"Embree", RTC_VERSION_STRING});
    info.thirdparty.push_back(
        {"Assimp", smdl::concat(aiGetVersionMajor(), ".", aiGetVersionMinor(),
                                ".", aiGetVersionPatch())});
    info.thirdparty.push_back(
        {"OpenSubdiv",
         smdl::concat(OPENSUBDIV_VERSION_MAJOR, ".", OPENSUBDIV_VERSION_MINOR,
                      ".", OPENSUBDIV_VERSION_PATCH)});
    os << info.toString();
  });
  cl::HideUnrelatedOptions({&catScene, &catUtility, &catSampling, &catCamera,
                            &catCameraLens, &catLight, &catTonemap,
                            &catOutput});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL toy renderer");
  // Honors '-print-options' and '-print-all-options', which LLVM
  // registers but leaves to the tool to act on; it prints nothing unless
  // one of them was given.
  cl::PrintOptionValues();
  // Before anything parallel: the thread pool is built by whichever
  // parallel operation runs first (the compile's image loads, usually)
  // and cannot be resized afterward. Embree keeps its own pool for
  // building acceleration structures, and `Scene` bounds that one from
  // `smdl::getThreadCount()`.
  smdl::setThreadCount(unsigned(optThreads));
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
  if (optAutolook && (optLookFrom.getNumOccurrences() > 0 ||
                      optLookTo.getNumOccurrences() > 0))
    throw smdl::Error("expected at most one of -autolook and "
                      "-look-from/-look-to (autolook solves the camera "
                      "position)");
  if (!(float(optAutolookZenith) >= 1 && float(optAutolookZenith) <= 179))
    throw smdl::Error("expected -autolook-zenith between 1 and 179");
  if (!(float(optAutolookMargin) >= 0 && float(optAutolookMargin) <= 0.5f))
    throw smdl::Error("expected -autolook-margin between 0 and 0.5");
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
      optWavelengthRange.getNumOccurrences() > 0)
    throw smdl::Error("expected at most one of -wavelengths and "
                      "-wavelength-range (they are two spellings of the "
                      "wavelength grid)");
  if (optRGBWaves.getNumOccurrences() > 0) {
    const auto waves{float3(optRGBWaves)};
    if (!(waves.x > 0 && waves.y > 0 && waves.z > 0))
      throw smdl::Error("expected -rgb-wavelengths to be three positive "
                        "wavelengths in nm");
  }
  if (!(std::isfinite(float(optShutterSpeed)) && float(optShutterSpeed) >= 0))
    throw smdl::Error("expected -shutter-speed to be finite and nonnegative");
  // Parsed and validated now so a typo fails before anything loads.
  const auto waveRange{
      parseWavelengthRangeFlag(std::string(optWavelengthRange))};
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
    layout.lights.insert(layout.lights.end(), more.lights.begin(),
                         more.lights.end());
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
  const auto pick{[](const auto &cliValueOpt, const auto &fileValue) {
    using ValueT = std::decay_t<decltype(cliValueOpt.getValue())>;
    return cliValueOpt.getNumOccurrences() == 0 && fileValue.has_value()
               ? ValueT(*fileValue)
               : ValueT(cliValueOpt);
  }};
  const auto &fileCamera{layout.camera};
  auto cameraOptions{CameraOptions{}};
  cameraOptions.resolution = pick(optResolution, fileCamera.resolution);
  cameraOptions.lookFrom = pick(optLookFrom, fileCamera.lookFrom);
  cameraOptions.lookTo = pick(optLookTo, fileCamera.lookTo);
  cameraOptions.lookUp = pick(optLookUp, fileCamera.lookUp);
  cameraOptions.fovYDeg = pick(optFOV, fileCamera.fovYDeg);
  cameraOptions.fStop = pick(optFStop, fileCamera.fStop);
  cameraOptions.aperture = pick(optAperture, fileCamera.aperture);
  cameraOptions.focus = pick(optFocus, fileCamera.focus);
  cameraOptions.blades = pick(optBlades, fileCamera.blades);
  cameraOptions.bladeAngleDeg = pick(optBladeAngle, fileCamera.bladeAngleDeg);
  cameraOptions.distortionK1 = pick(optDistortionK1, fileCamera.distortionK1);
  cameraOptions.distortionK2 = pick(optDistortionK2, fileCamera.distortionK2);
  cameraOptions.distortionFit =
      pick(optDistortionFit, fileCamera.distortionFit);
  cameraOptions.vignetting = pick(optVignetting, fileCamera.vignetting);
  cameraOptions.catEye = pick(optCatEye, fileCamera.catEye);
  cameraOptions.catEyeRadius = pick(optCatEyeRadius, fileCamera.catEyeRadius);
  cameraOptions.noLOD = bool(optNoLOD);
  // The same exclusivity the command line checks above, now that the file
  // has had its say: either source can supply either spelling, so only the
  // merged pair can be checked for naming both.
  if (cameraOptions.fStop > 0 && cameraOptions.aperture > 0)
    throw smdl::Error("expected at most one of -fstop and -aperture between "
                      "the command line and the scene file's 'camera' "
                      "directive (they are two spellings of the same "
                      "quantity)");
  // The two clocks, merged the same way from the layout's 'time'
  // directive. The parser has already refused a file value that is not
  // finite or, for the shutter, negative.
  renderTime() = pick(optTime, layout.time.base);
  renderShutter() = pick(optShutterSpeed, layout.time.shutter);
  // The camera's shut keys. The layout wrote them against its own
  // framing, so a flag that replaces that framing drops them rather
  // than moving a camera the file never described; a key the block
  // leaves unstated holds its open value.
  if (fileCamera.motion) {
    const char *framingFlag{optAutolook                           ? "-autolook"
                            : optLookFrom.getNumOccurrences() > 0 ? "-look-from"
                            : optLookTo.getNumOccurrences() > 0   ? "-look-to"
                            : optLookUp.getNumOccurrences() > 0   ? "-look-up"
                                                                  : nullptr};
    if (framingFlag) {
      SMDL_LOG_INFO("Camera motion: dropped, since ", framingFlag,
                    " replaces the framing the layout's 'motion' was "
                    "written against");
    } else if (!(renderShutter() > 0)) {
      SMDL_LOG_INFO("Camera motion: the shutter is shut, so the camera "
                    "holds its open framing");
    } else {
      const auto &motion{*fileCamera.motion};
      cameraOptions.motion = true;
      cameraOptions.lookFromShut =
          motion.lookFrom.value_or(cameraOptions.lookFrom);
      cameraOptions.lookToShut = motion.lookTo.value_or(cameraOptions.lookTo);
      cameraOptions.lookUpShut = motion.lookUp.value_or(cameraOptions.lookUp);
    }
  }
  // The instances' shut keys under a shut shutter are cleared, so
  // that every placement builds and renders through the static path,
  // which is what the layout without its 'motion' blocks renders.
  {
    size_t numMovingItems{};
    size_t numMovingLights{};
    for (const auto &item : layout.items)
      numMovingItems += item.objectToWorldShut || !item.batchXfsShut.empty();
    for (const auto &light : layout.lights)
      numMovingLights += light.lightToWorldShut.has_value();
    if (numMovingItems + numMovingLights > 0) {
      if (!(renderShutter() > 0)) {
        for (auto &item : layout.items) {
          item.objectToWorldShut.reset();
          item.batchXfsShut.clear();
        }
        for (auto &light : layout.lights) light.lightToWorldShut.reset();
        SMDL_LOG_INFO("Instance motion: the shutter is shut, so ",
                      numMovingItems, " placement(s) and ", numMovingLights,
                      " light(s) hold their open keys");
      } else {
        SMDL_LOG_INFO("Instance motion: ", numMovingItems, " placement(s) and ",
                      numMovingLights, " light(s) move over the shutter");
      }
    }
  }
  // Under -autolook the position comes from measuring the committed scene,
  // so construction (with the lens validation and the summary it logs)
  // waits until the solve below. Every other path keeps constructing
  // here, before anything slow loads, so a lens typo still fails fast.
  auto camera{std::optional<Camera>()};
  if (!optAutolook) camera.emplace(cameraOptions);
  const auto resolution{cameraOptions.resolution};
  const auto numPixelsX{size_t(resolution.x)};
  const auto numPixelsY{size_t(resolution.y)};
  const auto spp{size_t(optSPP)};
  // The pixel window to render, the whole frame unless -crop-window
  // narrows it.
  int4 window{0, 0, resolution.x, resolution.y};
  if (optCropWindow.getNumOccurrences() > 0) {
    window = int4(optCropWindow);
    if (!(0 <= window[0] && window[0] < window[2] &&
          window[2] <= resolution.x && 0 <= window[1] &&
          window[1] < window[3] && window[3] <= resolution.y))
      throw smdl::Error(
          smdl::concat("-crop-window ", spellVector(window),
                       " is not a non-empty sub-rectangle of -resolution ",
                       resolution.x, ",", resolution.y));
  }
  const auto numWindowPixels{size_t(window[2] - window[0]) *
                             size_t(window[3] - window[1])};
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
  // flag also implies -output-spectrum back to the same file; see the
  // output stage at the bottom.
  auto resumedFilm{smdl::SpectralFilm{}};
  auto resumed{smdl::SpectralFilm::ENVIFileInfo{}};
  bool resuming{false};
  const bool resumeRequested{!std::string(optResume).empty()};
  size_t sampleIndexBase{optSampleOffset};
  // The sample index the sequence BEGAN at, recorded in the spectrum
  // header and restored on resume, so that `-sample-offset` survives a
  // resumed session: a two-seed reference pair stays decorrelated only
  // if each seed's continuation keeps to its own stream.
  size_t sequenceSampleOffset{optSampleOffset};
  // How long the sequence has taken so far, over every session that has
  // rendered into it: wall clock, and the CPU time summed over all the
  // worker threads. Recorded in the spectrum header and added to below,
  // so that an image built over many resumed sessions still knows what
  // it cost. A file written before these fields existed starts the tally
  // at this session.
  double totalSeconds{};
  double totalCPUSeconds{};
  uint64_t numSessions{};
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
      // no 'smdl spp' field and could not itself be resumed.
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
    resumed = resumedFilm.readENVIFile(std::string(optResume));
    if (resumedFilm.getNumPixelsX() != numPixelsX ||
        resumedFilm.getNumPixelsY() != numPixelsY)
      throw smdl::Error(smdl::concat(
          "cannot resume: the file is ", resumedFilm.getNumPixelsX(), "x",
          resumedFilm.getNumPixelsY(), " against -resolution ", numPixelsX, ",",
          numPixelsY));
    if (resumed.samplesPerPixel == 0)
      throw smdl::Error("cannot resume: the header has no 'smdl spp' count "
                        "(the file was not written by -output-spectrum)");
    // The window is what the recorded count applies to, so a session
    // that moved it would accumulate over a different set of pixels and
    // the film would stop having a single samples per pixel. Both
    // directions land here: the file's window defaults to the whole
    // frame, and so does this session's.
    if (!smdl::isAllTrue(resumed.cropWindow == window))
      throw smdl::Error(smdl::concat(
          "cannot resume: the file was rendered with -crop-window ",
          spellVector(resumed.cropWindow), " against this session's ",
          spellVector(window),
          "; the window must be held constant across a resumed sequence, "
          "otherwise the samples per pixel stop being uniform"));
    if (auto itr{resumed.fields.find("render sampler")};
        itr == resumed.fields.end() || itr->second != SAMPLER_VERSION)
      SMDL_LOG_WARN(
          "resuming a file from a different sampler: the continuation "
          "samples are independent of the first session's rather than "
          "jointly stratified (still unbiased, noise just improves more "
          "slowly)");
    if (auto itr{resumed.fields.find("render wavelength jitter")};
        (itr != resumed.fields.end() && itr->second != "0") !=
        bool(optWavelengthJitter))
      SMDL_LOG_WARN(
          "resuming across a -wavelength-jitter change: a jittered band "
          "holds the mean radiance over the band and an unjittered one holds "
          "the radiance at one wavelength, so the merged image mixes two "
          "different quantities");
    if (auto itr{resumed.fields.find("render args")};
        itr != resumed.fields.end() &&
        stripSessionOnlyArgs(itr->second) != stripSessionOnlyArgs(argsEcho))
      SMDL_LOG_WARN("resuming with different flags: the file records ",
                    smdl::Quoted(itr->second),
                    "; if the scene or camera changed, the merged image "
                    "mixes two different renders");
    sequenceSampleOffset = 0;
    if (auto itr{resumed.fields.find("render sample offset")};
        itr != resumed.fields.end())
      sequenceSampleOffset =
          size_t(std::strtoull(itr->second.c_str(), nullptr, 10));
    const auto resumedSeconds{[&](const char *key) {
      auto itr{resumed.fields.find(key)};
      if (itr == resumed.fields.end()) return 0.0;
      const double seconds{std::strtod(itr->second.c_str(), nullptr)};
      return std::isfinite(seconds) && seconds > 0.0 ? seconds : 0.0;
    }};
    totalSeconds = resumedSeconds("render seconds");
    totalCPUSeconds = resumedSeconds("render cpu seconds");
    if (auto itr{resumed.fields.find("render sessions")};
        itr != resumed.fields.end())
      numSessions = uint64_t(std::strtoull(itr->second.c_str(), nullptr, 10));
    sampleIndexBase = sequenceSampleOffset + resumed.samplesPerPixel;
    SMDL_LOG_INFO("Resuming: ", resumed.samplesPerPixel,
                  " samples per pixel from ",
                  smdl::Quoted(std::string(optResume)), " (sample offset ",
                  sequenceSampleOffset, ")");
  }
  // The wavelength grid, in priority order: explicit '-wavelengths',
  // '-wavelength-range' uniform bands (endpoint-inclusive), or, when
  // resuming with no grid flags at all, the grid recorded in the resumed
  // file, so a resumed render needs no grid retyping. The band count
  // seeds every 'Color' constructed from here on.
  const bool haveGridFlags{optWavelengthRange.getNumOccurrences() > 0 ||
                           optWavelengths.getNumOccurrences() > 0};
  const bool adoptResumedGrid{!haveGridFlags && resuming};
  auto gridSpec{explicitWavelengths};
  if (adoptResumedGrid) {
    if (resumed.wavelengths.empty())
      throw smdl::Error(
          "cannot resume: the file carries no wavelengths to adopt, give "
          "the grid explicitly with -wavelength-range or -wavelengths");
    gridSpec = resumed.wavelengths;
  }
  if (gridSpec.empty()) {
    gridSpec.resize(size_t(waveRange.bandCount));
    for (size_t i = 0; i < gridSpec.size(); i++) {
      const float t{float(i) / float(gridSpec.size() - 1)};
      gridSpec[i] = (1 - t) * waveRange.range.x + t * waveRange.range.y;
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
  // The jitter rectangles; see `renderWavelengthBandEdges()`. Left empty
  // when the flag is off, which is what the renderer tests, so a default
  // render never asks.
  {
    auto &edges{renderWavelengthBandEdges()};
    edges.clear();
    if (optWavelengthJitter) {
      edges = wavelengthBandEdges(
          smdl::Span<const float>(gridSpec.data(), gridSpec.size()));
      if (edges.empty())
        SMDL_LOG_WARN("-wavelength-jitter needs at least 2 bands to have a "
                      "band width to jitter within, so it does nothing here");
    }
  }
  const auto wavelengths{
      Color(smdl::Span<const float>(gridSpec.data(), gridSpec.size()))};
  renderWavelengths() = wavelengths;
  if (resuming) {
    if (resumedFilm.getNumBands() != wavelengths.size())
      throw smdl::Error(smdl::concat(
          "cannot resume: the file has ", resumedFilm.getNumBands(),
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
  // The spectral extent the render actually reaches, which the jitter
  // widens to the outermost band edges.
  const auto &bandEdges{renderWavelengthBandEdges()};
  const float gridLower{bandEdges.empty() ? wavelengths[0] : bandEdges.front()};
  const float gridUpper{bandEdges.empty() ? wavelengths[wavelengths.size() - 1]
                                          : bandEdges.back()};
  if (!bandEdges.empty())
    SMDL_LOG_INFO("Wavelength jitter: bands tile ", gridLower, "-", gridUpper,
                  " nm, each holding its own mean");
  if (wavelengths.size() > 256)
    SMDL_LOG_WARN(wavelengths.size(),
                  " bands: JIT compile time and per-sample cost both grow "
                  "with the band count, expect a slow start and a slow "
                  "render");
  // Everything RGB-sourced degrades outside the visible; say so once
  // rather than rendering a mysteriously dark image.
  const bool gridBeyondVisible{gridLower < 379.0f || gridUpper > 781.0f};
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
  compiler.enableUnitTests = false;
  registerSceneData(compiler);
  // The normal distribution entry points are what a glossy manifold
  // crossing draws its half vector from, and nothing else here asks for
  // them, so they are emitted only when that is on.
  bool anyCaster{false};
  for (const auto &item : layout.items) anyCaster |= item.isCaster;
  compiler.enableScatterNormal =
      (optMNEE && anyCaster) || optMNEETestNormalHook;
  // The built-in stand-in, always available: a scene whose materials
  // have not been written yet still renders, and a name that does not
  // resolve has somewhere to fall back to. It is added even when MDL
  // modules are given, so that '-fallback-material default_object' works
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
  auto fallbackMaterial{std::string(optFallbackMaterial)};
  if (fallbackMaterial.empty() && optInputMDLFiles.empty())
    fallbackMaterial = DEFAULT_OBJECT_MATERIAL_NAME;
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
  BoundBox3 guideBound{};
  if (optGround || optGroundZ.getNumOccurrences() > 0) {
    guideBound = scene.preCommitBounds();
    guideBoundsValid = true;
    if (guideBound.isEmpty())
      throw smdl::Error("cannot -ground: the scene has no geometry to "
                        "put a plane under");
    const float z{optGroundZ.getNumOccurrences() > 0 ? float(optGroundZ)
                                                     : guideBound.lower.z};
    // Large enough that at autolook elevations the plane's edge lands at
    // the visual horizon, small enough to stay in float precision.
    const float halfExtent{std::clamp(
        1000.0f * 0.5f * smdl::length(guideBound.extent()), 100.0f, 20000.0f)};
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
  // The autolook solve, and the deferred camera construction it exists
  // for. The solved azimuth also becomes the default sun azimuth below,
  // so a batch of thumbnails is consistently lit however each one is
  // framed.
  auto autolookSunAzimuth{std::optional<float>()};
  if (optAutolook) {
    auto autolookOptions{AutolookOptions{}};
    autolookOptions.fovYDeg = cameraOptions.fovYDeg;
    autolookOptions.aspectRatio = float(resolution.x) / float(resolution.y);
    autolookOptions.zenithDeg = float(optAutolookZenith);
    if (optAutolookAzimuth.getNumOccurrences() > 0) {
      autolookOptions.azimuthDeg = float(optAutolookAzimuth);
    } else if (layout.frontAzimuth) {
      autolookOptions.azimuthDeg = layout.frontAzimuth;
      SMDL_LOG_INFO("Autolook: locked to the manifest's front azimuth ",
                    *layout.frontAzimuth, " degrees");
    }
    autolookOptions.margin = float(optAutolookMargin);
    autolookOptions.ignoreBackfaces = bool(optAutolookIgnoreBackfaces);
    autolookOptions.skipInstance = groundInstance;
    const auto autolook{solveAutolook(scene, autolookOptions)};
    cameraOptions.lookFrom = autolook.lookFrom;
    cameraOptions.lookTo = autolook.lookTo;
    camera.emplace(cameraOptions);
    // The key light over the camera's right shoulder.
    autolookSunAzimuth = autolook.azimuthDeg - 35.0f;
  }

  // The environment, merged from the same three sources as the camera and
  // in the same order: the defaults, the layout's 'sky' directive, and
  // whatever the command line explicitly gave.
  const auto &fileSky{layout.sky};
  const auto iblFileName{pick(optIBLFilename, fileSky.iblFileName)};
  const auto moonGiven{optMoonPhase.getNumOccurrences() > 0 ||
                       bool(fileSky.moonPhase)};
  std::unique_ptr<EnvLight> envLight{};
  if (!iblFileName.empty()) {
    envLight = std::make_unique<EnvLight>(iblFileName,
                                          pick(optIBLScale, fileSky.iblScale));
    if (gridBeyondVisible)
      SMDL_LOG_WARN("-ibl is an RGB image: on this wavelength grid it "
                    "contributes only inside the visible");
  } else if (!pick(optNoSunSky, fileSky.none)) {
    auto options{smdl::SunSkyOptions{}};
    float zenith{smdl::radians(pick(optSunZenith, fileSky.sunZenith))};
    float azimuthDeg{pick(optSunAzimuth, fileSky.sunAzimuth)};
    // Under -autolook with no stated sun azimuth, the key light follows the
    // solved camera: a perfectly framed thumbnail lit from behind is as
    // unreadable as one framed end-on, and this keeps a whole library
    // consistently lit however each asset was framed.
    if (autolookSunAzimuth && optSunAzimuth.getNumOccurrences() == 0 &&
        !fileSky.sunAzimuth) {
      azimuthDeg = *autolookSunAzimuth;
      SMDL_LOG_INFO("Sun azimuth follows the framed camera: ", azimuthDeg,
                    " degrees");
    }
    const float azimuth{smdl::radians(azimuthDeg)};
    options.sunDirection =
        float3(std::sin(zenith) * std::cos(azimuth),
               std::sin(zenith) * std::sin(azimuth), std::cos(zenith));
    options.visibility = pick(optSkyVisibility, fileSky.visibility);
    options.waterVaporScale = pick(optSkyWaterVapor, fileSky.waterVapor);
    options.scaleFactor = pick(optSkyScale, fileSky.scale);
    if (moonGiven) {
      options.moon = true;
      options.moonPhase = pick(optMoonPhase, fileSky.moonPhase);
      options.moonDistanceScale = pick(optMoonDistance, fileSky.moonDistance);
    }
    envLight = std::make_unique<EnvLight>(options);
  }

  // The exterior medium the layout's 'medium' directive names, if
  // any: one material instance evaluated up front in an allocator that
  // outlives the render, seeding every camera path's medium stack. The
  // instance has no geometry, so heterogeneous coefficient queries run
  // in world space directly. Evaluated once at the base animation time,
  // so an open shutter does not vary its captured homogeneous
  // coefficients (per-point heterogeneous queries still see the path
  // time).
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

  // The exterior haze: the analytic exponential-height atmosphere that
  // produces aerial perspective, whose extinction, transmittance and
  // free-flight distance are all closed form, so it costs no tracking
  // and no majorant. It is the medium of everything outside all
  // geometry, which is where the 'medium' directive puts its material
  // too, so the two cannot both be asked for.
  const auto &fileHaze{layout.haze};
  bool hazeEnabled{optHaze || layout.hasHaze};
  if (pick(optNoHaze, fileHaze.none)) hazeEnabled = false;
  std::unique_ptr<smdl::Haze> haze{};
  if (hazeEnabled) {
    if (exteriorMedium)
      throw smdl::Error("the exterior haze and the 'medium' directive both "
                        "describe the medium outside all geometry; keep one");
    auto options{smdl::HazeOptions{}};
    // An unwritten visibility follows the sky's, so that distant
    // terrain does not read hazier or clearer than the horizon sky
    // immediately behind it. The two models overlap toward the sky; see
    // `LayoutHaze`.
    options.visibility = pick(optHazeVisibility, fileHaze.visibility);
    if (!(options.visibility > 0.0f))
      options.visibility = pick(optSkyVisibility, fileSky.visibility);
    options.scaleHeight = pick(optHazeScaleHeight, fileHaze.scaleHeight);
    if (fileHaze.baseHeight) options.baseHeight = *fileHaze.baseHeight;
    if (fileHaze.droplet) options.dropletSize = *fileHaze.droplet;
    haze = std::make_unique<smdl::Haze>(
        options,
        smdl::Span<const float>(wavelengths.data(), wavelengths.size()),
        makeRenderState(wavelengths).meters_per_scene_unit);
    SMDL_LOG_INFO("Exterior haze: visibility ", options.visibility,
                  " km, scale height ", options.scaleHeight, " m");
  }
  // Every light in one selection path: each emissive mesh instance plus
  // the environment, weighted by power.
  auto profLightSampler{smdl::profilerEntryBegin("Build light sampler")};
  const auto lights{LightSampler(compiler, scene, envLight.get(), layout.lights,
                                 wavelengths, optAllLights, !optNoLightTree)};
  smdl::profilerEntryEnd(profLightSampler);
  // The render loop is deliberately outside the trace; see -profile.
  if (profiling) smdl::profilerFinalize(profileFileName.c_str());
  auto film{smdl::SpectralFilm(wavelengths.size(), numPixelsX, numPixelsY)};
  // -resume implies writing back to the file being resumed, so one
  // command line re-runs to keep accumulating; an explicitly given
  // -output-spectrum wins verbatim, redirecting or (when empty)
  // suppressing the write.
  const auto outputSpectrum{optOutputSpectrum.getNumOccurrences() > 0 ||
                                    !resumeRequested
                                ? std::string(optOutputSpectrum)
                                : std::string(optResume)};
  // Will this session leave a guide tree behind? Whenever guiding is on
  // and the spectrum accumulation is being written: the tree pairs with
  // that file, and a session resuming it inherits the training.
  const bool savingTree{bool(optGuide) && spp > 0 && !outputSpectrum.empty()};
  auto sdtree{std::unique_ptr<STree>()};
  // How many samples per pixel trained the resumed tree, 0 without one:
  // what the pass schedule continues from.
  size_t guideTrainedSpp{0};
  if (optGuide && resuming) {
    // Resume the guide tree saved beside the accumulation, so this
    // session starts guided by everything the sequence has learned. The
    // tree only steers sampling, so a missing or unreadable one is
    // never fatal: retraining from scratch is always safe, just slower
    // to converge.
    const auto treeName{std::string(optResume) +
                        std::string(GUIDE_TREE_EXTENSION)};
    if (smdl::exists(treeName)) {
      try {
        uint64_t treeSpp{};
        sdtree = std::make_unique<STree>(STree::readFile(treeName, treeSpp));
        guideTrainedSpp = size_t(treeSpp);
        SMDL_LOG_INFO("Resuming guide tree: ", smdl::Quoted(treeName), ", ",
                      sdtree->leafCount(), " spatial leaves trained by ",
                      treeSpp, " spp");
        if (treeSpp != resumed.samplesPerPixel)
          SMDL_LOG_WARN("The guide tree was trained by ", treeSpp,
                        " spp against the accumulation's ",
                        resumed.samplesPerPixel,
                        "; using it anyway, since a tree that is behind "
                        "still guides");
      } catch (const smdl::Error &error) {
        SMDL_LOG_WARN("Cannot resume guide tree, retraining from scratch: ",
                      error.message);
      }
    } else {
      SMDL_LOG_INFO("No guide tree at ", smdl::Quoted(treeName),
                    ", retraining from scratch");
    }
  }
  if (optGuide && !sdtree) {
    // With a ground plane, guide over the actual geometry padded by half
    // its own size, so the plane's enormous backdrop extent does not
    // dilute the spatial resolution; vertices on the far plane clamp
    // into the border cells, where there is nothing worth guiding
    // anyway. Without one, the scene bounds are the geometry bounds.
    auto center{scene.boundCenter};
    auto r{scene.boundRadius};
    if (guideBoundsValid) {
      center = guideBound.center();
      r = 0.75f * smdl::length(guideBound.extent());
    }
    sdtree = std::make_unique<STree>(center - float3(r, r, r),
                                     center + float3(r, r, r));
    SMDL_LOG_INFO("Guide bounds: center (", center.x, ", ", center.y, ", ",
                  center.z, "), radius ", r);
  }
  // The combination of the guided passes, which also maintains the
  // ADRRS pixel estimates between passes. Null without guiding, where
  // the single pass accumulates straight into `film`.
  auto combiner{std::unique_ptr<PassCombiner>()};
  if (optGuide) {
    combiner = std::make_unique<PassCombiner>(numPixelsX, numPixelsY, window);
    // Seed with the prior session's accumulation, so resolve() below
    // reproduces the full merged image (the unguided path adds it into
    // the accumulation instead, just below) and the first pass's ADRRS
    // starts from the resumed estimates rather than zero.
    if (resuming) {
      combiner->seed(resumedFilm);
      combiner->rebuildPixelEstimates();
    }
  }
  // Merge a resumed session's samples in before rendering rather than
  // after it, so that the previews written along the way already stand on
  // every sample taken and the image is never displayed noisier than it
  // is. One image-level add, which is exactly the merge the
  // sums-plus-count invariant makes safe; every read below divides by
  // the combined count.
  if (resuming && !combiner) film.add(resumedFilm);
  // Nothing reads it again, and it is the same size as the film being
  // rendered into.
  resumedFilm.clear();
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
  // watching the file sees the image converge. The sums-plus-count film
  // is a valid mean at every moment, so a checkpoint is the finished write
  // with fewer samples behind it, and nothing about the estimator changes.
  // Written beside the output and renamed into place: a watcher polling
  // the path never opens a half-written PNG.
  const double previewEvery{std::max(double(optPreviewEvery), 0.0)};
  const bool isCheckpointing{previewEvery > 0.0 &&
                             !std::string(optOutputRGB).empty()};
  const auto writeDisplayImage{[&] {
    // Resolve first, so that a guided preview stands on every pass folded
    // so far, the resumed seed included, instead of the newest pass
    // alone. Guided checkpoints only happen on pass boundaries, where the
    // pass just rendered is already folded in.
    if (combiner) combiner->resolve(film);
    const auto path{std::filesystem::path(std::string(optOutputRGB))};
    auto partPath{path};
    partPath.replace_extension("part" + path.extension().string());
    const auto rgb{resolveRGB(compiler, film, wavelengths, rgbPolicy)};
    const auto ldr{tonemap(tonemapOptions, rgb, film, wavelengths)};
    if (auto error{smdl::write8bitImage(partPath.string(), //
                                        int(numPixelsX), int(numPixelsY), 3,
                                        ldr.data())}) {
      error->print();
      return;
    }
    // A checkpoint that loses the rename is one missed preview, not a
    // reason to stop the render.
    (void)smdl::tryRenameOnto(partPath.string(), path.string());
  }};
  auto lastCheckpoint{std::chrono::steady_clock::now()};
  const auto checkpoint{[&] {
    if (!isCheckpointing) return;
    const auto now{std::chrono::steady_clock::now()};
    if (std::chrono::duration<double>(now - lastCheckpoint).count() <
        previewEvery)
      return;
    writeDisplayImage();
    // Timed from the end of the write, so that an image expensive to tone
    // map spaces its checkpoints out instead of running back to back.
    lastCheckpoint = std::chrono::steady_clock::now();
  }};

  const auto passes{solveSamplePasses(spp, bool(optGuide), guideTrainedSpp)};
  // The manifold-NEE chain depth `tracePath()` runs with, 0 when
  // disabled.
  const int mneeDepth{optMNEE ? int(std::clamp(unsigned(optMNEEDepth), 1U,
                                               unsigned(MANIFOLD_MAX_DEPTH)))
                              : 0};
  MNEEOptions mneeOptions{};
  mneeOptions.depth = mneeDepth;
  ManifoldStats::global().setEnabled(bool(optMNEEReport));
  mneeOptions.maxTrials = int(std::max(unsigned(optMNEEMaxTrials), 1U));
  mneeOptions.biasedTrials = int(unsigned(optMNEEBiased));
  mneeOptions.maxRoughness = std::max(float(optMNEEMaxRoughness), 0.0f);
  mneeOptions.minReceiverAlpha = std::max(float(optMNEEReceiverAlpha), 0.0f);
  // The reflective gather searches this in place of the straight shadow
  // segment, so it is built once per render: the layout's marked casters,
  // with what each claims.
  auto mneeCasters{MNEECasterSet()};
  if (optMNEE) {
    mneeCasters = MNEECasterSet(scene, wavelengths, mneeOptions.maxRoughness);
    mneeOptions.casters = &mneeCasters;
    SMDL_LOG_DEBUG("MNEE casters: ", mneeCasters.casters.size(),
                   " instance(s)");
    if (optMNEESunOnly && envLight)
      mneeOptions.sunOnly =
          envLight->sunCone(mneeOptions.sunDirection, mneeOptions.cosSunRadius);
  }
  // The default walk is terminated by Russian roulette, with the bounce
  // bound set high enough that clipping it is negligible even for
  // high-albedo transport; giving -max-bounces makes the bound the whole
  // termination rule, so the estimate is the fixed-depth truncation.
  PathOptions pathOptions{};
  pathOptions.maxBounces = unsigned(optMaxBounces);
  pathOptions.useRoulette = optMaxBounces.getNumOccurrences() == 0;
  pathOptions.maxContribution = std::max(float(optMaxContribution), 0.0f);
  pathOptions.maxContributionBounces =
      int(std::max(unsigned(optMaxContributionBounces), 1U));
  // Whether every sample draws its own wavelength grid; see
  // `renderWavelengthBandEdges()` and `jitterWavelengths()`.
  const bool jitterWavelength{!renderWavelengthBandEdges().empty()};
  if (optMNEETestNormalHook) {
    std::cout << "Checking the geometry-normal hook against the meshes:\n";
    const int failures{runMNEETestNormalHook(scene)};
    if (failures == 0)
      std::cout << "All unmapped instances agree\n";
    else
      std::cout << failures << " instance(s) disagree\n";
    return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
  }
  progressOptions.total = numWindowPixels * spp;
  progressOptions.displayScale = std::max<size_t>(spp, 1);
  progressOptions.summary =
      optCropWindow.getNumOccurrences() > 0
          ? smdl::concat("Rendered window ", spellVector(window), " of ",
                         numPixelsX, "x", numPixelsY, " at ", spp, " spp")
          : smdl::concat("Rendered ", numPixelsX, "x", numPixelsY, " at ", spp,
                         " spp");
  // The render window the cumulative times above measure: the sample
  // passes and the previews written between them, but none of the setup
  // that came before or the outputs that come after, so that the number
  // means the same thing in every session of a resumed sequence.
  const auto renderStartWall{std::chrono::steady_clock::now()};
  const double renderStartCompute{cpuTimeSeconds()};
  ProgressBar progress{progressOptions};
  size_t sppDone{0};
  size_t chunkSpp{1};
  for (size_t passIndex = 0; passIndex < passes.size(); passIndex++) {
    const size_t thisPass{passes[passIndex]};
    const bool isFinal{passIndex + 1 == passes.size()};
    if (optGuide)
      progress.setNote(
          smdl::concat("pass ", passIndex + 1, "/", passes.size()));
    // Pre-final passes train the SD-tree; every pass contributes to the
    // output through the pass combination below. When the tree will be
    // saved the final pass trains too: its training is no longer wasted,
    // it is what the next session of the sequence inherits.
    const bool recordPass{optGuide && (!isFinal || savingTree)};
    // The per-thread training mirrors for this pass, absorbed into the
    // tree after the pass renders and before it refines; the tree
    // structure the layout mirrors is frozen in between.
    auto guideAccumulator{std::unique_ptr<GuideAccumulator>()};
    if (recordPass)
      guideAccumulator = std::make_unique<GuideAccumulator>(*sdtree);
    // Without guiding the whole budget is one pass, so checkpointing has
    // to split it; the chunk starts at one sample, so the first image
    // lands almost immediately, and then grows toward the interval asked
    // for. With guiding the passes are the chunks: they already grow
    // geometrically, and splitting one would change what the combiner
    // weights.
    const bool isChunked{isCheckpointing && !combiner};
    for (size_t passDone{0}; passDone < thisPass;) {
      const size_t chunk{isChunked ? std::min(chunkSpp, thisPass - passDone)
                                   : thisPass - passDone};
      const size_t chunkBase{passDone};
      const auto chunkStart{std::chrono::steady_clock::now()};
      smdl::parallelFor(0, numWindowPixels, [&](size_t k) {
        // Denormals are worth flushing for the whole task: the material
        // code the walk runs produces them, and the microcode assist each one
        // costs is a measurable fraction of the render.
        const smdl::ScopedFlushDenormals flushDenormals{};
        // The pixel index in the whole frame, which seeds the sampler and
        // addresses every per-pixel buffer, so a window renders the same
        // pixels the whole frame would.
        const size_t windowWidth{size_t(window[2] - window[0])};
        const size_t i{(size_t(window[1]) + k / windowWidth) * numPixelsX +
                       (size_t(window[0]) + k % windowWidth)};
        const size_t x{i % numPixelsX};
        const size_t y{i / numPixelsX};
        // Constructed per pixel deliberately: hoisting this to a
        // thread_local measures as pure noise (the few malloc/free pairs
        // per pixel amortize across worker threads and malloc's own thread
        // cache), so the simpler lifetime wins.
        smdl::BumpPtrAllocator allocator;
        Sampler sampler;
        // Training records for `trainGuiding()`, one per vertex the walk
        // may reach, constructed only on the pre-final guiding passes
        // that fill them: at a runtime band count every record holds
        // sized vectors, too much to pay per pixel of a non-guiding
        // render.
        std::vector<GuideRecord> guideRecords;
        if (recordPass) guideRecords.resize(pathOptions.maxBounces + 1);
        // The sample's own wavelength grid, rewritten in place once per
        // sample: a `Color` past `SpectralColor::INLINE_CAPACITY` bands
        // heaps, and every state built from it holds the pointer rather
        // than a copy, so one buffer per pixel serves the whole sample.
        std::optional<Color> jittered;
        if (jitterWavelength) jittered.emplace(wavelengths);
        Color Lsum{};
        PassCombiner::PixelHalves halves{};
        Guiding guiding{};
        guiding.tree = sdtree.get();
        guiding.pixelEstimate =
            combiner && optGuideADRRS ? combiner->pixelEstimate(i) : 0.0f;
        guiding.bsdfFraction =
            std::clamp(float(optGuideBSDFFraction), 0.0f, 1.0f);
        guiding.bsdfFractionFixed =
            optGuideBSDFFraction.getNumOccurrences() > 0;
        for (size_t s = 0; s < chunk; s++) {
          const uint32_t sampleIndex =
              sampleIndexBase + sppDone + chunkBase + s;
          sampler.startPixelSample(uint32_t(i), sampleIndex);
          if (jitterWavelength)
            jitterWavelengths(*jittered,
                              wavelengthJitterOffset(uint32_t(i), sampleIndex));
          const Color &sampleWavelengths{jitterWavelength ? *jittered
                                                          : wavelengths};
          Color Lsample{};
          // A fully vignetted sample contributes nothing, so skip the
          // walk but let it still count in the average below, keeping the
          // darkening unbiased.
          uint64_t numRecords{0};
          if (auto cameraSample{camera->sample(x, y, sampler)};
              cameraSample.weight > 0) {
            // The path's time. The shutter fraction is drawn only when
            // the shutter is open, matching the lens-point precedent, so
            // a default render's sampler sequence is unchanged; the
            // camera ray is placed in the world only now, at that time.
            float shutterFraction{};
            if (renderShutter() > 0) shutterFraction = float(sampler);
            const PathTime time{shutterFraction};
            camera->toWorld(cameraSample, time.fraction);
            Lsample = tracePath(
                compiler, allocator, scene, sampler, sampleWavelengths,
                cameraSample.ray, time, cameraSample.weight,
                cameraSample.coneAngle, exteriorMedium, haze.get(), lights,
                mneeOptions, pathOptions, &guiding,
                recordPass ? guideRecords.data() : nullptr, numRecords);
          }
          // Train the SD-tree on the records the walk retained.
          if (recordPass && numRecords > 0)
            trainGuiding(*sdtree, *guideAccumulator, sampler,
                         guideRecords.data(), numRecords);
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
        // With guiding the combination owns the film and resolves into
        // it, pass by pass; without, the accumulation is the film.
        if (combiner) {
          combiner->deposit(i, halves);
        } else {
          film.addTotals(x, y, Lsum.data());
        }
        // Counted where the work is finished rather than where it starts,
        // which at thumbnail sizes is a whole pool's worth of pixels.
        progress.advance(chunk);
      });
      // Every pixel of the window took the same samples, so the count
      // belongs to the film rather than to each pixel, and is recorded
      // once here where the chunk is finished. It has to land before the
      // checkpoint below, which divides by it.
      if (!combiner) film.addSamples(chunk);
      passDone += chunk;
      if (isChunked) {
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
        chunkSpp = std::clamp<size_t>(wanted, 1, chunk * 4);
        checkpoint();
      }
    }
    if (combiner) combiner->foldPass(thisPass);
    if (recordPass) {
      guideAccumulator->absorbInto(*sdtree);
      combiner->rebuildPixelEstimates();
      // Refine: split spatial leaves past c*sqrt(2^k) records (k this
      // pass's index), rebuild the directional quadtrees with the 1% flux
      // threshold.
      sdtree->refine(uint32_t(double(optGuideSplit) * std::sqrt(thisPass)),
                     0.01f, 20);
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
  // A '-spp 0' re-run of the output stage rendered nothing, so it is not
  // a session and must leave the totals it rewrites exactly as they were.
  if (spp > 0) {
    totalSeconds += std::chrono::duration<double>(
                        std::chrono::steady_clock::now() - renderStartWall)
                        .count();
    totalCPUSeconds += std::max(cpuTimeSeconds() - renderStartCompute, 0.0);
    numSessions++;
  }
  if (optMNEEReport) ManifoldStats::global().print(std::cout);
  // Resolve the pass combination back into the film every downstream
  // output reads from. A resumed session's samples are already in there,
  // through the seeded combination or the add before the render.
  if (combiner) combiner->resolve(film);
  const auto rgbImage{resolveRGB(compiler, film, wavelengths, rgbPolicy)};
  if (!std::string(optOutputRGBf).empty()) {
    if (auto error{smdl::writeFloatImage(std::string(optOutputRGBf),
                                         int(numPixelsX), int(numPixelsY), 3,
                                         rgbImage.data())}) {
      error->print();
    }
  }
  if (!outputSpectrum.empty()) {
    // TODO If using procedural SunSky (and not in moonlight mode), and standard ENVI header lines:
    // sun azimuth = (degrees)
    // sun elevation = (degrees)
    // solar irradiance = {...} (W/m2/um)

    // Write through a temporary and rename, so an interrupted write
    // cannot destroy the file a resumed session reads from, which may
    // be this very path.
    const auto partName{outputSpectrum + ".part"};
    const auto extraHeaderLines{std::vector<std::string>{
        smdl::concat("render sessions = ", numSessions),
        smdl::concat("render seconds = ", totalSeconds),
        smdl::concat("render cpu seconds = ", totalCPUSeconds),
        smdl::concat("render sampler = ", SAMPLER_VERSION),
        smdl::concat("render sample offset = ", sequenceSampleOffset),
        smdl::concat("render wavelength jitter = ",
                     jitterWavelength ? "1" : "0"),
        smdl::concat("render args = ", argsEcho)}};
    // The window the recorded count belongs to, which the film itself
    // does not know: a windowed render still carries a full frame of
    // pixels, and the header must not describe the untouched ones as
    // samples.
    film.writeENVIFile(
        smdl::Span<const float>(wavelengths.data(), wavelengths.size()),
        partName, extraHeaderLines, window);
    // Both members of the ENVI pair; `writeENVIFile()` wrote them under
    // the temporary name and its own '.hdr' suffix.
    smdl::renameOnto(partName, outputSpectrum);
    smdl::renameOnto(partName + ".hdr", outputSpectrum + ".hdr");
    if (savingTree) {
      // The guide tree rides beside the accumulation with the same
      // temporary-and-rename discipline, stamped with the merged sample
      // count so a resumed session can tell how far behind a stale tree
      // is.
      const auto treeName{outputSpectrum + std::string(GUIDE_TREE_EXTENSION)};
      const auto treePartName{treeName + ".part"};
      sdtree->writeFile(treePartName, resumed.samplesPerPixel + spp);
      smdl::renameOnto(treePartName, treeName);
      SMDL_LOG_INFO("Wrote guide tree: ", smdl::Quoted(treeName), ", ",
                    sdtree->leafCount(), " spatial leaves");
    }
    SMDL_LOG_INFO("Cumulative render time: ", formatDuration(totalSeconds),
                  " wall, ", formatDuration(totalCPUSeconds), " compute over ",
                  numSessions, " session(s)");
  }
  {
    const auto ldrImage{tonemap(tonemapOptions, rgbImage, film, wavelengths)};
    if (auto error{smdl::write8bitImage(std::string(optOutputRGB),
                                        int(numPixelsX), int(numPixelsY), 3,
                                        ldrImage.data())}) {
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
