// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iterator>

#include "assimp/version.h"
#include "embree4/rtcore_config.h"
#include "opensubdiv/version.h"

#include "../CommandLine.h"
#include "llvm/Support/WithColor.h"

#include "smdl/Common.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "Options.h"
#include "Render/Manifold.h"
namespace {

// Optional rather than required only because the '.places' utility
// flags below run without a scene; everything else checks it by hand
// right after they bow out.
cl::opt<std::string> optInputSceneFile{cl::Positional,
                                       cl::desc("<input scene>")};
cl::list<std::string> optInputMDLFiles{cl::Positional, cl::desc("<input mdl>"),
                                       cl::ZeroOrMore};

cl::OptionCategory catCamera{"Camera Options"};
//--{ Camera Options
cl::opt<std::string> optCameraFile{
    "camera",
    cl::desc("The '.camera' file: the framing, the lens, the sensor, and the "
             "shot (default: the '.camera' beside the input layout, if there "
             "is one)"),
    cl::cat(catCamera)};
cl::opt<float3> optLookFrom{
    "look-from",
    cl::desc("The position to look from, overriding the camera file's "
             "'look_from' (default: the camera file's, else -6,0,2)"),
    cl::init(float3{-6, 0, 2}), cl::cat(catCamera)};
cl::opt<float3> optLookTo{
    "look-to",
    cl::desc("The position to look to, overriding the camera file's "
             "'look_to' (default: the camera file's, else 0,0,0.5)"),
    cl::init(float3{0, 0, 0.5}), cl::cat(catCamera)};
cl::opt<float3> optLookUp{
    "look-up",
    cl::desc("The up vector, overriding the camera file's 'look_up' "
             "(default: the camera file's, else 0,0,1)"),
    cl::init(float3{0, 0, 1}), cl::cat(catCamera)};
cl::opt<bool> optIdeal{
    "ideal",
    cl::desc("Preview the camera: a '.lens' becomes the thin lens fitted to "
             "it, a '.sensor' body becomes the CIE observer on the body's "
             "frame and pixels, and the picture is exposed as the body would "
             "expose it"),
    cl::init(false), cl::cat(catCamera)};
cl::opt<bool> optAutolook{
    "autolook",
    cl::desc("Solve -look-from/-look-to to fit the scene at the camera's "
             "field of view"),
    cl::init(false), cl::cat(catCamera)};
// Kept apart from -autolook on purpose: folding the azimuth into an
// optional value of -autolook would make LLVM demand '-autolook=N' (a
// space never binds the value) and hide a bare -autolook from
// -print-options.
cl::opt<float> optAutolookAzimuth{
    "autolook-azimuth",
    cl::desc("With -autolook, the azimuth of the scene-to-camera direction in "
             "degrees CCW from +X (default: solved for frame fill)"),
    cl::init(0.0f), cl::cat(catCamera)};
cl::opt<float> optAutolookZenith{
    "autolook-zenith",
    cl::desc("With -autolook, the zenith angle of the scene-to-camera "
             "direction in degrees (default: 65, the standard 3/4 view)"),
    cl::init(65.0f), cl::cat(catCamera)};
cl::opt<float> optAutolookMargin{
    "autolook-margin",
    cl::desc("With -autolook, the padding to the frame edge as a fraction of "
             "the frame (default: 0.05)"),
    cl::init(0.05f), cl::cat(catCamera)};
cl::opt<bool> optAutolookIgnoreBackfaces{
    "autolook-ignore-backfaces",
    cl::desc("With -autolook, neither avoid nor warn about views of backfacing "
             "geometry"),
    cl::init(false), cl::cat(catCamera)};
cl::opt<std::string> optISO{
    "iso",
    cl::desc("The ISO a physical sensor is read out at, or 'auto' to meter it "
             "from the rendered film as the saturation speed of ISO 12232, "
             "never below the body's base ISO, overriding the camera file's "
             "'iso' (default: the camera file's, else auto)"),
    cl::cat(catCamera)};
cl::opt<std::string> optWhiteBalance{
    "white-balance",
    cl::desc("The white a physical sensor's develop balances to: D65, "
             "daylight, cloudy, shade, tungsten, fluorescent, a color "
             "temperature in kelvin, or 'auto' for the frame's gray world, "
             "overriding the camera file's 'white_balance' (default: the "
             "camera file's, else D65)"),
    cl::cat(catCamera)};
//--}

cl::OptionCategory catCompile{"Compile Options"};
//--{ Compile Options
cl::opt<unsigned> optOptLevel{"O",
                              cl::desc("The optimization level (default: 2)"),
                              cl::Prefix, cl::init(2U), cl::cat(catCompile)};
cl::opt<bool> optDebug{"g", cl::desc("Enable debugging"), cl::init(false),
                       cl::cat(catCompile)};
cl::opt<std::string> optWavelengthRange{
    "wavelength-range",
    cl::desc("Wavelengths spanning A to B nm with N bands, "
             "format 'A,B:N' where ':N' is optional (default: 380,720:16)"),
    cl::cat(catCompile)};
cl::opt<std::string> optWavelengths{
    "wavelengths",
    cl::desc("Wavelengths in nm, comma-separated or a text file of "
             "whitespace-separated values (mutually exclusive with "
             "-wavelength-range)"),
    cl::cat(catCompile)};
//--}

cl::OptionCategory catImage{"Image Options"};
//--{ Image Options
cl::opt<int2> optResolution{
    "resolution",
    cl::desc("The image dimensions in pixels (default: 1280,720)"),
    cl::init(int2{1280, 720}), cl::cat(catImage)};
cl::opt<float> optResolutionScale{
    "resolution-scale",
    cl::desc("Render this fraction of the frame's pixels, a smaller picture "
             "of the same frame, for the observer and under -ideal; a body "
             "renders exactly its own pixels (default: 1)"),
    cl::init(1.0f), cl::cat(catImage)};
cl::opt<int4> optCropWindow{
    "crop-window",
    cl::desc("Render only pixels x0 <= x < x1, y0 <= y < y1 of the -resolution "
             "frame, given as x0,y0,x1,y1 (default: the whole frame)\n"
             "* the output keeps the full size with the rest black"),
    cl::init(int4{0, 0, 0, 0}), cl::cat(catImage)};
cl::opt<float> optExposure{
    "exposure",
    cl::desc("The exposure applied before tone mapping (default: 1)"),
    cl::init(1.0f), cl::cat(catImage)};
cl::opt<bool> optFalseColor{
    "false-color",
    cl::desc("Force false color band mapping for the RGB outputs\n"
             "* engages automatically when wavelength grid does not cover the "
             "visible"),
    cl::init(false), cl::cat(catImage)};
cl::opt<float3> optRGBWavelengths{
    "rgb-wavelengths",
    cl::desc("With -false-color, the wavelengths in nm mapped to R,G,B "
             "(default: 5/6, 1/2, and 1/6 of the grid span, long to red)"),
    cl::init(float3{}), cl::cat(catImage)};
cl::opt<bool> optMedianFilter{
    "median-filter",
    cl::desc("Replace firefly pixels with a neighbor in the RGB outputs\n"
             "* the spectral output stays radiometric, and so does the "
             "sequence a later -resume reads"),
    cl::init(false), cl::cat(catImage)};
cl::opt<float> optMedianFilterFactor{
    "median-filter-factor",
    cl::desc("With -median-filter, how many times its neighborhood a pixel "
             "must exceed to be replaced (default: 8)\n"
             "* 4 is aggressive and 16 is cautious; the render reports what "
             "it replaced"),
    cl::init(8.0f), cl::cat(catImage)};
cl::opt<int> optMedianFilterRadius{
    "median-filter-radius",
    cl::desc("With -median-filter, the neighborhood radius in pixels "
             "(default: 1, a 3x3 window)\n"
             "* raise it only for a block of fireflies wider than the window"),
    cl::init(1), cl::cat(catImage)};
cl::opt<std::string> optOutputRGB{
    "output-rgb",
    cl::desc("The tone mapped RGB image filename (default: output.png)"),
    cl::init(std::string("output.png")), cl::cat(catImage)};
cl::opt<std::string> optOutputRGBf{
    "output-rgbf",
    cl::desc("Also write linear RGB radiance to this '.exr' or '.hdr' file"),
    cl::cat(catImage)};
cl::opt<std::string> optOutputSpectrum{
    "output-spectrum",
    cl::desc("Also write linear spectral radiance to this ENVI file"),
    cl::cat(catImage)};
cl::opt<bool> optOutputSpectrumDouble{
    "output-spectrum-double",
    cl::desc("Write -output-spectrum, and the band film beside it, as 64-bit "
             "floats rather than 32-bit\n"
             "* for a byte-for-byte comparison of two renders: a 32-bit mean "
             "is far finer than any render's noise, and -resume reads "
             "either"),
    cl::cat(catImage)};
cl::opt<std::string> optResume{
    "resume",
    cl::desc("Resume accumulating from this ENVI file written by a previous "
             "-output-spectrum"),
    cl::cat(catImage)};
cl::opt<std::string> optOutputDN{
    "output-dn",
    cl::desc("Also write the detector readout to this 16-bit ENVI file of "
             "digital numbers, through the camera's '.sensor' body"),
    cl::cat(catImage)};
cl::opt<unsigned> optDetectorSeed{
    "detector-seed",
    cl::desc("With -output-dn, which noise realization to draw (default: 0)"),
    cl::init(0), cl::cat(catImage)};
cl::opt<std::string> optDetectorNoise{
    "detector-noise",
    cl::desc("With -output-dn, which noise to draw: 'none', 'shot', or "
             "'all' (default: all), each in full on the film's mean, which "
             "the readout takes as converged\n"
             "* 'none' makes the digital numbers a function of the film "
             "alone\n"
             "* 'shot' draws the shot noise alone\n"
             "* 'all' adds the read noise"),
    cl::init(std::string("all")), cl::cat(catImage)};
cl::opt<std::string> optTonemap{
    "tonemap",
    cl::desc(
        R"(The tonemap for 8-bit output as stages joined by '+', e.g., 'filmic+fusion', 'night+log:6', 'filmic+fusion:0.5,2' (default: gamma)
* 'gamma' clamps and gamma-encodes
* 'log:DECADES' maps the decades below the exposure-scaled white point (default: 4)
* 'filmic' rolls highlights off toward white instead of clipping them
* 'night' models vision at absolute luminance and auto-exposes, for physically dim scenes like moonlight
* 'fusion:STRENGTH,CLAMP,SPAN' auto-exposes locally, leaving -exposure a relative adjustment; how
  much local exposure to keep from 0 to 1, the largest local deviation in EV, and the bracket's total
  span in EV or 0 to infer (default: 0.75, 3, 0))"),
    cl::init(std::string("gamma")), cl::cat(catImage)};
//--}

cl::OptionCategory catLight{"Light Options"};
//--{ Light Options
cl::opt<bool> optNoSunSky{
    "no-sky",
    cl::desc("Disable the default sun-sky, restoring the black "
             "environment"),
    cl::init(false), cl::cat(catLight)};
cl::opt<float> optSunZenith{
    "sun-zenith",
    cl::desc("The solar zenith angle in degrees, 5-88 (default: 42)"),
    cl::init(42.0f), cl::cat(catLight)};
cl::opt<float> optSunAzimuth{
    "sun-azimuth",
    cl::desc("The solar azimuth angle in degrees CCW from +X (default: 135)"),
    cl::init(135.0f), cl::cat(catLight)};
cl::opt<float> optSkyVisibility{
    "sky-visibility",
    cl::desc("The aerosol visibility in km, 5-100 (default: 23)"),
    cl::init(23.0f), cl::cat(catLight)};
cl::opt<float> optSkyWaterVapor{
    "sky-water-vapor",
    cl::desc("The water-vapor column scale factor, 0.3-3 (default: 1)"),
    cl::init(1.0f), cl::cat(catLight)};
cl::opt<bool> optHaze{
    "haze",
    cl::desc("Enable the exterior haze that produces aerial perspective, "
             "which a layout's 'haze' block configures"),
    cl::init(false), cl::cat(catLight)};
cl::opt<bool> optNoHaze{
    "no-haze", cl::desc("Disable the exterior haze a layout asked for"),
    cl::init(false), cl::cat(catLight)};
cl::opt<float> optHazeVisibility{
    "haze-visibility",
    cl::desc("The haze meteorological range in km at 550nm (default: "
             "-sky-visibility)"),
    cl::init(0.0f), cl::cat(catLight)};
cl::opt<float> optHazeScaleHeight{
    "haze-scale-height",
    cl::desc("The haze scale height in meters (default: 2100)"),
    cl::init(2100.0f), cl::cat(catLight)};
cl::opt<float> optSkyScale{
    "sky-scale", cl::desc("The sky radiance scale factor (default: 1)"),
    cl::init(1.0f), cl::cat(catLight)};
cl::opt<float> optMoonPhase{
    "moon",
    cl::desc("Enable moonlight mode at this signed phase angle in degrees\n"
             "* 0 is full, +/-180 is new, the sign picks waxing or waning\n"
             "* radiance is ~1e-6 of daylight, use with '-tonemap night'"),
    cl::init(0.0f), cl::cat(catLight)};
cl::opt<float> optMoonDistance{
    "moon-distance",
    cl::desc("With -moon, the lunar distance factor (default: 1, realistic "
             "range ~0.86-1.14)"),
    cl::init(1.0f), cl::cat(catLight)};
cl::opt<std::string> optIBLFilename{
    "ibl",
    cl::desc("The IBL filename (any supported format, likely '.hdr', '.exr')"),
    cl::cat(catLight)};
cl::opt<float> optIBLScale{
    "ibl-scale", cl::desc("With -ibl, the IBL scale factor (default: 1)"),
    cl::init(1.0f), cl::cat(catLight)};
//--}

cl::OptionCategory catRendering{"Rendering Options"};
//--{ Rendering Options
cl::opt<unsigned> optSPP{
    "spp", cl::desc("The number of samples per pixel (default: 8)"),
    cl::init(8U), cl::cat(catRendering)};
cl::opt<unsigned> optSampleOffset{
    "sample-offset",
    cl::desc("The sample index this render starts from, to decorrelate renders "
             "(default: 0, -resume overrides)"),
    cl::init(0), cl::cat(catRendering)};
cl::opt<unsigned> optMaxBounces{
    "max-bounces",
    cl::desc("Trace every path to at most this many bounces with no Russian "
             "roulette (default: roulette, backstopped at 63)\n"
             "* a bounce is a scattering event: 0 keeps only the emission the "
             "camera sees directly, 1 adds direct lighting"),
    cl::init(63U), cl::cat(catRendering)};
cl::opt<float> optMaxContribution{
    "max-contribution",
    cl::desc("Limit any single contribution to this per-band radiance "
             "(default: 0, off)"),
    cl::init(0.0f), cl::cat(catRendering)};
cl::opt<unsigned> optMaxContributionBounces{
    "max-contribution-bounces",
    cl::desc("With -max-contribution, only bound contributions of at least "
             "this many bounces (default: 1)"),
    cl::init(1U), cl::cat(catRendering)};
cl::opt<bool> optGuide{"guide", cl::desc("Enable SD-tree path guiding"),
                       cl::init(false), cl::cat(catRendering)};
cl::opt<bool> optGuideADRRS{
    "guide-adrrs",
    cl::desc("With -guide, drive Russian roulette by expected pixel "
             "contribution instead of throughput (default: true)\n"
             "* moot with -max-bounces, which turns roulette off"),
    cl::init(true), cl::cat(catRendering)};
cl::opt<float> optGuideBSDFFraction{
    "guide-bsdf-fraction",
    cl::desc("With -guide, probability of sampling the BSDF instead of "
             "the SD-tree at guided vertices (default: 0.5)"),
    cl::init(0.5f), cl::cat(catRendering)};
cl::opt<float> optGuideSplit{
    "guide-split",
    cl::desc("With -guide, SD-tree spatial split threshold in records "
             "(default: 12000)"),
    cl::init(12000.0f), cl::cat(catRendering)};
cl::opt<bool> optMNEE{"mnee", cl::desc("Enable manifold next-event estimation"),
                      cl::init(false), cl::cat(catRendering)};
cl::opt<unsigned> optMNEEDepth{
    "mnee-depth",
    cl::desc("With -mnee, maximum number of refractive interfaces a "
             "connection may cross, 1 to 4 (default: 4)"),
    cl::init(4), cl::cat(catRendering)};
cl::opt<unsigned> optMNEEMaxTrials{
    "mnee-max-trials",
    cl::desc("With -mnee, max attempts to re-find a reciprocal "
             "estimate before dropping the sample (default: 256)"),
    cl::init(256), cl::cat(catRendering)};
cl::opt<float> optMNEEReceiverAlpha{
    "mnee-receiver-alpha",
    cl::desc("With -mnee, squared roughness needed to be a "
             "receiver (default: 0.005, 0 takes every finite lobe)"),
    cl::init(0.005f), cl::cat(catRendering)};
cl::opt<unsigned> optMNEEBiased{
    "mnee-biased",
    cl::desc("With -mnee, enable biased mode with this many walks per estimate "
             "(default: 0, unbiased)"),
    cl::init(0), cl::cat(catRendering)};
cl::opt<float> optMNEEMaxRoughness{
    "mnee-max-roughness",
    cl::desc("With -mnee, do not claim glossy lobes with roughness wider than "
             "this (default: 0, no limit)"),
    cl::init(0.0f), cl::cat(catRendering)};
cl::opt<bool> optMNEESunOnly{
    "mnee-sun-only",
    cl::desc("With -mnee and procedural sun-sky, restrict the Dirac-chain "
             "machinery to the sun disk"),
    cl::init(false), cl::cat(catRendering)};
cl::opt<bool> optReport{
    "report",
    cl::desc("Print path and contribution statistics after the render, to "
             "choose -max-bounces and -max-contribution from data\n"
             "* with -mnee, the manifold estimator statistics as well\n"
             "* with -json, print them as one JSON document instead"),
    cl::init(false), cl::cat(catRendering)};
cl::opt<bool> optMNEETestNormalHook{
    "mnee-test-normalhook",
    cl::desc("Test the geometry-normal hook against the meshes and exit, "
             "non-zero on failure"),
    cl::init(false), cl::cat(catRendering)};
cl::opt<bool> optMarkAllLights{
    "mark-all-lights",
    cl::desc("Aim light selection at every emissive surface, marked 'light' "
             "in the layout or not (a scene given without a layout marks "
             "everything already)"),
    cl::init(false), cl::cat(catRendering)};
cl::opt<bool> optNoLOD{
    "no-lod", cl::desc("Disable LOD by zeroing the camera ray cone spread"),
    cl::init(false), cl::cat(catRendering)};
cl::opt<bool> optNoLightTree{
    "no-light-tree",
    cl::desc("Use flat power-weighted light distribution instead of the "
             "spatial tree"),
    cl::init(false), cl::cat(catRendering)};
cl::opt<bool> optNoRobustIntersection{
    "no-robust-intersection",
    cl::desc("Trace against the faster ray-triangle test instead of the "
             "watertight one"),
    cl::init(false), cl::cat(catRendering)};
cl::opt<bool> optWavelengthJitter{
    "wavelength-jitter",
    cl::desc("Jitter each wavelength to estimate the mean radiance over the "
             "band rather than the radiance at one wavelength\n"
             "* the outermost bands reach half a band past the grid ends"),
    cl::init(false), cl::cat(catRendering)};
//--}

cl::OptionCategory catScene{"Scene Options"};
//--{ Scene Options
cl::opt<float> optTime{
    "time",
    cl::desc("The instant to render, as the animation time in seconds at "
             "shutter open (default: 0)\n"
             "* the only source: a scene file states when things happen, "
             "never which instant to photograph"),
    cl::init(0.0f), cl::cat(catScene)};
cl::list<std::string> optInputMeshFiles{
    "mesh-file", cl::desc("Add another mesh, repeatable"), cl::cat(catScene)};
cl::list<std::string> optAssetDirs{
    "asset-dir",
    cl::desc("Add a directory to search for assets and meshes, repeatable"),
    cl::cat(catScene)};
cl::opt<bool> optGround{"ground",
                        cl::desc("Add a ground plane under the scene"),
                        cl::init(false), cl::cat(catScene)};
cl::opt<float> optGroundZ{
    "ground-z",
    cl::desc("Place the ground plane at this height (implies -ground)"),
    cl::init(0.0f), cl::cat(catScene)};
cl::opt<std::string> optGroundMaterial{
    "ground-material",
    cl::desc("With -ground, the MDL material for the ground plane (default: 10 "
             "percent gray)"),
    cl::cat(catScene)};
cl::opt<std::string> optFallbackMaterial{
    "fallback-material",
    cl::desc("The MDL material for names the scene does not resolve "
             "(default: none, an error)\n"
             "* 'default_object' is built in, a plain 20 percent Lambertian"),
    cl::cat(catScene)};
//--}

cl::OptionCategory catUtility{"Utility Options"};
//--{ Utility Options
// NOTE: There is deliberately no '-color' here, though 'smdl' has one.
// LLVM registers a '--color' of its own for `WithColor`, lazily from
// `HideUnrelatedOptions()`, and `cl` aborts outright on a duplicate
// name. 'smdl' gets away with one only because it scopes it to
// subcommands, which are searched ahead of the top level; this program
// has no subcommands, so the name is simply taken. Coloring here stays
// autodetected, and LLVM's own '--color' drives nothing but `WithColor`.
cl::opt<std::string> optLogLevel{
    "log-level",
    cl::desc("The log level to filter output verbosity, must be "
             "'debug', 'info', 'warn', or 'error' (default: 'info')"),
    cl::init(std::string("info")), cl::cat(catUtility)};
cl::opt<cl::boolOrDefault> optUnicode{
    "unicode",
    cl::desc("Label log messages and draw the progress bar with Unicode "
             "symbols rather than ASCII (default: autodetect)"),
    cl::init(cl::boolOrDefault::BOU_UNSET), cl::cat(catUtility)};
cl::opt<std::string> optDumpPlaces{
    "dump-places",
    cl::desc("Print '.places' buffer as one-line place text, then exit"),
    cl::cat(catUtility)};
cl::opt<std::string> optDumpCurves{
    "dump-curves", cl::desc("Print '.curves' file summary, then exit"),
    cl::cat(catUtility)};
cl::opt<std::string> optPackPlaces{
    "pack-places",
    cl::desc("Pack layout's 'place' statements into a '.places' buffer, "
             "then exit"),
    cl::cat(catUtility)};
cl::opt<std::string> optOutputPlaces{
    "pack-places-file",
    cl::desc(
        "The output file for -pack-places (default: layout name + '.places')"),
    cl::cat(catUtility)};
cl::opt<bool> optListMaterials{
    "list-materials", cl::desc("List material names the scene needs and exit"),
    cl::init(false), cl::cat(catUtility)};
cl::opt<bool> optListObjects{
    "list-objects",
    cl::desc("List objects present in each scene file and exit"),
    cl::init(false), cl::cat(catUtility)};
cl::opt<bool> optDescribeCamera{
    "describe-camera",
    cl::desc("Print what the camera resolves to and exit, before any scene "
             "is read: the frame, the lens and its ideal fit, the focus, the "
             "body, the exposure, the dynamic range, and the color fit; "
             "needs no scene when -camera is given"),
    cl::init(false), cl::cat(catUtility)};
cl::opt<bool> optJSON{"json",
                      cl::desc("With -list-objects, -list-materials, or "
                               "-report, print JSON instead of a table"),
                      cl::init(false), cl::cat(catUtility)};
cl::opt<bool> optCompileAllMaterials{
    "compile-all-materials",
    cl::desc("Compile every material in the given MDL modules unconditionally"),
    cl::init(false), cl::cat(catUtility)};
cl::opt<unsigned> optThreads{
    "threads",
    cl::desc("Set the thread limit, or 0 for the maximum (default: 0)\n"
             "* '-threads 1' runs inline with no pool at all, for a debugger"),
    cl::init(0), cl::cat(catUtility)};
cl::opt<std::string> optProfile{
    "profile",
    cl::desc("Write a time-trace JSON of everything before rendering starts "
             "(default: smdl-toy.trace.json)\n"
             "* open in chrome://tracing or https://ui.perfetto.dev"),
    cl::ValueOptional, cl::init(std::string{}), cl::cat(catUtility)};
cl::opt<bool> optProgress{
    "progress",
    cl::desc("Draw a progress bar while rendering, if stderr is a terminal "
             "(default: true)"),
    cl::init(true), cl::cat(catUtility)};
cl::opt<std::string> optProgressFile{
    "progress-file",
    cl::desc("Write 'done=N total=M elapsed=S eta=S note=...' progress into "
             "this file, about ten times a second"),
    cl::cat(catUtility)};
cl::opt<double> optPreviewEvery{
    "preview-every",
    cl::desc("Rewrite '-output-rgb' about this often in seconds "
             "(default: 0, off)"),
    cl::init(0.0), cl::cat(catUtility)};
//--}

} // namespace

Options parseCommandLine(int argc, char **argv) {
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
  cl::HideUnrelatedOptions({&catCamera, &catCompile, &catImage, &catLight,
                            &catRendering, &catScene, &catUtility});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL toy renderer");
  // Honors '-print-options' and '-print-all-options', which LLVM
  // registers but leaves to the tool to act on; it prints nothing unless
  // one of them was given.
  cl::PrintOptionValues();
  // The ISO, in its two forms: a number, or the meter.
  auto iso{Flag<float>{}};
  bool shouldMeterISO{};
  if (optISO.getNumOccurrences() > 0) {
    const auto &text{std::string(optISO)};
    if (text == "auto") {
      shouldMeterISO = true;
    } else {
      char *end{};
      const float value{std::strtof(text.c_str(), &end)};
      if (text.empty() || *end != '\0' || !std::isfinite(value) || !(value > 0))
        throw smdl::Error(smdl::concat(
            "expected -iso to be a positive number or 'auto', got ",
            smdl::Quoted(text)));
      iso = Flag<float>{value, true};
    }
  }
  auto whiteBalance{Flag<WhiteBalance>{}};
  if (optWhiteBalance.getNumOccurrences() > 0) {
    const auto &text{std::string(optWhiteBalance)};
    const auto parsed{parseWhiteBalance(text)};
    if (!parsed)
      throw smdl::Error(smdl::concat(
          "expected -white-balance to be D65, daylight, cloudy, shade, "
          "tungsten, fluorescent, auto, or a color temperature from ",
          int(WHITE_BALANCE_KELVIN_MIN), " to ", int(WHITE_BALANCE_KELVIN_MAX),
          " K, got ", smdl::Quoted(text)));
    whiteBalance = Flag<WhiteBalance>{*parsed, true};
  }
  if (!(float(optResolutionScale) > 0 && float(optResolutionScale) <= 1))
    throw smdl::Error("expected -resolution-scale to be greater than 0 and at "
                      "most 1");
  if (optAutolook && (optLookFrom.getNumOccurrences() > 0 ||
                      optLookTo.getNumOccurrences() > 0))
    throw smdl::Error("expected at most one of -autolook and "
                      "-look-from/-look-to (autolook solves the camera "
                      "position)");
  if (!(float(optAutolookZenith) >= 1 && float(optAutolookZenith) <= 179))
    throw smdl::Error("expected -autolook-zenith between 1 and 179");
  if (!(float(optAutolookMargin) >= 0 && float(optAutolookMargin) <= 0.5f))
    throw smdl::Error("expected -autolook-margin between 0 and 0.5");
  if (optWavelengths.getNumOccurrences() > 0 &&
      optWavelengthRange.getNumOccurrences() > 0)
    throw smdl::Error("expected at most one of -wavelengths and "
                      "-wavelength-range (they are two spellings of the "
                      "wavelength grid)");
  // The shared parser admits a single band; a render wants a band width
  // to jitter and to integrate over, so it does not.
  if (parseWavelengthRange(std::string(optWavelengthRange)).bandCount < 2)
    throw smdl::Error("expected -wavelength-range ':N' to be at least 2");
  if (optRGBWavelengths.getNumOccurrences() > 0) {
    const auto waves{float3(optRGBWavelengths)};
    if (!(waves.x > 0 && waves.y > 0 && waves.z > 0))
      throw smdl::Error("expected -rgb-wavelengths to be three positive "
                        "wavelengths in nm");
  }
  if (!(float(optMedianFilterFactor) > 1))
    throw smdl::Error("expected -median-filter-factor to be greater than 1");
  if (!(int(optMedianFilterRadius) >= 1 &&
        int(optMedianFilterRadius) <= MEDIAN_FILTER_MAX_RADIUS))
    throw smdl::Error(smdl::concat("expected -median-filter-radius between 1 "
                                   "and ",
                                   MEDIAN_FILTER_MAX_RADIUS));
  if (!std::isfinite(float(optTime)))
    throw smdl::Error("expected -time to be finite");

  auto opts{Options{}};

  opts.compile.optLevel = smdl::OptLevel(std::min(unsigned(optOptLevel), 3U));
  opts.compile.isDebugEnabled = bool(optDebug);

  opts.camera.file = std::string(optCameraFile);
  opts.camera.lookFrom = flag(optLookFrom);
  opts.camera.lookTo = flag(optLookTo);
  opts.camera.lookUp = flag(optLookUp);
  opts.camera.isIdeal = bool(optIdeal);
  opts.camera.iso = iso;
  opts.camera.shouldMeterISO = shouldMeterISO;
  opts.camera.whiteBalance = whiteBalance;
  opts.camera.autolook.isEnabled = bool(optAutolook);
  opts.camera.autolook.azimuthDeg = flag(optAutolookAzimuth);
  opts.camera.autolook.zenithDeg = float(optAutolookZenith);
  opts.camera.autolook.margin = float(optAutolookMargin);
  opts.camera.autolook.ignoreBackfaces = bool(optAutolookIgnoreBackfaces);

  opts.image.resolution = flag(optResolution);
  opts.image.resolutionScale = flag(optResolutionScale);
  opts.image.cropWindow = flag(optCropWindow);
  opts.image.rgbPolicy.shouldForceFalseColor =
      bool(optFalseColor) || optRGBWavelengths.getNumOccurrences() > 0;
  if (optRGBWavelengths.getNumOccurrences() > 0) {
    const auto waves{float3(optRGBWavelengths)};
    opts.image.rgbPolicy.falseColorWaves = {waves.x, waves.y, waves.z};
  }
  opts.image.tonemap = parseTonemapOptions(std::string(optTonemap));
  opts.image.tonemap.exposure = float(optExposure);
  opts.image.medianFilter.isEnabled = bool(optMedianFilter);
  opts.image.medianFilter.factor = float(optMedianFilterFactor);
  opts.image.medianFilter.radius = int(optMedianFilterRadius);
  opts.image.outputRGB = std::string(optOutputRGB);
  opts.image.outputRGBFloat = std::string(optOutputRGBf);
  opts.image.outputSpectrum = std::string(optOutputSpectrum);
  opts.image.wasOutputSpectrumGiven = optOutputSpectrum.getNumOccurrences() > 0;
  opts.image.shouldWriteDouble = optOutputSpectrumDouble;
  opts.image.resume = std::string(optResume);
  opts.image.outputDN = std::string(optOutputDN);
  opts.image.readout.seed = unsigned(optDetectorSeed);
  opts.image.readout.noise = parseDetectorNoise(std::string(optDetectorNoise));

  opts.light.sky.none = flag(optNoSunSky);
  opts.light.sky.sunZenithDeg = flag(optSunZenith);
  opts.light.sky.sunAzimuthDeg = flag(optSunAzimuth);
  opts.light.sky.visibility = flag(optSkyVisibility);
  opts.light.sky.waterVapor = flag(optSkyWaterVapor);
  opts.light.sky.scale = flag(optSkyScale);
  opts.light.sky.moonPhase = flag(optMoonPhase);
  opts.light.sky.moonDistance = flag(optMoonDistance);
  opts.light.sky.iblFileName = flag(optIBLFilename);
  opts.light.sky.iblScale = flag(optIBLScale);
  opts.light.haze.isOn = bool(optHaze);
  opts.light.haze.none = flag(optNoHaze);
  opts.light.haze.visibility = flag(optHazeVisibility);
  opts.light.haze.scaleHeight = flag(optHazeScaleHeight);

  opts.render.sampling.spp = unsigned(optSPP);
  opts.render.sampling.sampleOffset = unsigned(optSampleOffset);
  opts.render.sampling.noLOD = bool(optNoLOD);
  // The default walk is terminated by Russian roulette, with the bounce
  // bound set high enough that clipping it is negligible even for
  // high-albedo transport; giving -max-bounces makes the bound the whole
  // termination rule, so the estimate is the fixed-depth truncation.
  opts.render.path.maxBounces = unsigned(optMaxBounces);
  opts.render.path.useRoulette = optMaxBounces.getNumOccurrences() == 0;
  opts.render.path.maxContribution = std::max(float(optMaxContribution), 0.0f);
  opts.render.path.maxContributionBounces =
      int(std::max(unsigned(optMaxContributionBounces), 1U));
  opts.render.guide.isEnabled = bool(optGuide);
  opts.render.guide.useADRRS = bool(optGuideADRRS);
  opts.render.guide.bsdfFraction = flag(optGuideBSDFFraction);
  opts.render.guide.split = float(optGuideSplit);
  // Parsed here so a typo fails before anything loads.
  opts.render.grid.range =
      parseWavelengthRange(std::string(optWavelengthRange));
  opts.render.grid.explicitWavelengths =
      parseWavelengths(std::string(optWavelengths));
  opts.render.grid.wasGiven = optWavelengthRange.getNumOccurrences() > 0 ||
                              optWavelengths.getNumOccurrences() > 0;
  opts.render.grid.shouldJitter = flag(optWavelengthJitter);
  // The manifold estimator, minus what needs a scene.
  opts.render.useMNEE = bool(optMNEE);
  opts.render.shouldReportStats = bool(optReport);
  opts.render.useMNEESunOnly = bool(optMNEESunOnly);
  opts.render.shouldTestMNEENormalHook = bool(optMNEETestNormalHook);
  opts.render.mnee.depth = optMNEE
                               ? int(std::clamp(unsigned(optMNEEDepth), 1U,
                                                unsigned(MANIFOLD_MAX_DEPTH)))
                               : 0;
  opts.render.mnee.maxTrials = int(std::max(unsigned(optMNEEMaxTrials), 1U));
  opts.render.mnee.biasedTrials = int(unsigned(optMNEEBiased));
  opts.render.mnee.maxRoughness = std::max(float(optMNEEMaxRoughness), 0.0f);
  opts.render.mnee.minReceiverAlpha =
      std::max(float(optMNEEReceiverAlpha), 0.0f);
  opts.render.allLights = bool(optMarkAllLights);
  opts.render.noLightTree = bool(optNoLightTree);
  opts.render.noRobustIntersection = bool(optNoRobustIntersection);

  opts.scene.inputSceneFile = std::string(optInputSceneFile);
  opts.scene.inputMDLFiles.assign(optInputMDLFiles.begin(),
                                  optInputMDLFiles.end());
  opts.scene.inputMeshFiles.assign(optInputMeshFiles.begin(),
                                   optInputMeshFiles.end());
  opts.scene.assetDirs.assign(optAssetDirs.begin(), optAssetDirs.end());
  opts.scene.time = float(optTime);
  opts.scene.hasGround = bool(optGround);
  opts.scene.groundZ = flag(optGroundZ);
  opts.scene.groundMaterial = std::string(optGroundMaterial);
  opts.scene.fallbackMaterial = std::string(optFallbackMaterial);

  opts.utility.dumpPlaces = std::string(optDumpPlaces);
  opts.utility.dumpCurves = std::string(optDumpCurves);
  opts.utility.packPlaces = std::string(optPackPlaces);
  opts.utility.outputPlaces = std::string(optOutputPlaces);
  opts.utility.shouldListMaterials = bool(optListMaterials);
  opts.utility.shouldListObjects = bool(optListObjects);
  opts.utility.shouldDescribeCamera = bool(optDescribeCamera);
  opts.utility.useJSON = bool(optJSON);
  opts.utility.allMaterials = bool(optCompileAllMaterials);
  opts.utility.threads = unsigned(optThreads);
  opts.utility.logLevel = parseLogLevel(std::string(optLogLevel));
  opts.utility.unicodeMode = lowerUnicodeMode(optUnicode);
  opts.utility.progress.label = "Rendering";
  opts.utility.progress.units = "px";
  opts.utility.progress.shouldDraw = bool(optProgress);
  opts.utility.progress.unicodeMode = opts.utility.unicodeMode;
  opts.utility.progress.filePath = std::string(optProgressFile);
  opts.utility.previewEvery = double(optPreviewEvery);
  opts.utility.profile = std::string(optProfile).empty()
                             ? std::string("smdl-toy.trace.json")
                             : std::string(optProfile);
  opts.utility.isProfiling = optProfile.getNumOccurrences() > 0;

  // The command line as it was given, for the spectral output's
  // 'render args' field.
  for (int i = 1; i < argc; i++) {
    if (i > 1) opts.argsEcho += ' ';
    opts.argsEcho += argv[i];
  }
  return opts;
}
