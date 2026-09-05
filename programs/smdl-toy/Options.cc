#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iterator>

#include "assimp/version.h"
#include "embree4/rtcore_config.h"
#include "opensubdiv/version.h"

#include "CommandLine.h"
#include "llvm/Support/WithColor.h"

#include "smdl/Common.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

#include "Options.h"
#include "Render/Manifold.h"

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
namespace {

// A `cl::opt` lowered: the value, and whether the command line gave it.
template <typename T> [[nodiscard]] Flag<T> flag(const cl::opt<T> &option) {
  return Flag<T>{T(option), option.getNumOccurrences() > 0};
}

// Parse the '-wavelengths' flag: wavelengths in nanometers separated by
// commas or whitespace, or the name of a text file of the same, which
// wins whenever the value opens as a file. NOT '@file': LLVM's command
// line expands '@'-prefixed argv tokens as response files before any
// option sees them. Returns empty when the flag was not given; anything
// else must be a finite, positive, strictly increasing list.
[[nodiscard]]
std::vector<float> parseWavelengthsFlag(const std::string &flagValue) {
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

// Parse the '-wavelength-range' flag: 'A,B:N' for N uniform bands
// spanning A to B nm, with ':N' optional. Returns the default grid when
// the flag was not given.
[[nodiscard]]
WavelengthRange parseWavelengthRangeFlag(const std::string &flagValue) {
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
  cl::HideUnrelatedOptions({&catScene, &catUtility, &catSampling, &catCamera,
                            &catCameraLens, &catLight, &catTonemap,
                            &catOutput});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL toy renderer");
  // Honors '-print-options' and '-print-all-options', which LLVM
  // registers but leaves to the tool to act on; it prints nothing unless
  // one of them was given.
  cl::PrintOptionValues();
  // Validate the occurrence-dependent lens flags here at the CLI, where
  // "was this given at all" is knowable; in the `CameraOptions` built
  // later zero means unset, so an explicit value has to be positive to
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

  auto opts{Options{}};
  opts.utility.dumpPlaces = std::string(optDumpPlaces);
  opts.utility.dumpCurves = std::string(optDumpCurves);
  opts.utility.packPlaces = std::string(optPackPlaces);
  opts.utility.outputPlaces = std::string(optOutputPlaces);
  opts.utility.listMaterials = bool(optListMaterials);
  opts.utility.listObjects = bool(optListObjects);
  opts.utility.json = bool(optJSON);

  opts.scene.inputSceneFile = std::string(optInputSceneFile);
  opts.scene.inputMDLFiles.assign(optInputMDLFiles.begin(),
                                  optInputMDLFiles.end());
  opts.scene.inputMeshFiles.assign(optInputMeshFiles.begin(),
                                   optInputMeshFiles.end());
  opts.scene.assetDirs.assign(optAssetDirs.begin(), optAssetDirs.end());
  opts.scene.allMaterials = bool(optAllMaterials);
  opts.scene.ground = bool(optGround);
  opts.scene.groundZ = flag(optGroundZ);
  opts.scene.groundMaterial = std::string(optGroundMaterial);
  opts.scene.fallbackMaterial = std::string(optFallbackMaterial);

  opts.shutter.time = flag(optTime);
  opts.shutter.speed = flag(optShutterSpeed);

  opts.sampling.spp = unsigned(optSPP);
  opts.sampling.sampleOffset = unsigned(optSampleOffset);
  opts.sampling.threads = unsigned(optThreads);
  opts.sampling.noLOD = bool(optNoLOD);

  opts.guide.enabled = bool(optGuide);
  opts.guide.adrrs = bool(optGuideADRRS);
  opts.guide.bsdfFraction = flag(optGuideBSDFFraction);
  opts.guide.split = float(optGuideSplit);

  opts.camera.resolution = flag(optResolution);
  opts.camera.cropWindow = flag(optCropWindow);
  opts.camera.lookFrom = flag(optLookFrom);
  opts.camera.lookTo = flag(optLookTo);
  opts.camera.lookUp = flag(optLookUp);
  opts.camera.fovYDeg = flag(optFOV);
  opts.camera.fStop = flag(optFStop);
  opts.camera.aperture = flag(optAperture);
  opts.camera.focus = flag(optFocus);
  opts.camera.blades = flag(optBlades);
  opts.camera.bladeAngleDeg = flag(optBladeAngle);
  opts.camera.distortionK1 = flag(optDistortionK1);
  opts.camera.distortionK2 = flag(optDistortionK2);
  opts.camera.distortionFit = flag(optDistortionFit);
  opts.camera.vignetting = flag(optVignetting);
  opts.camera.catEye = flag(optCatEye);
  opts.camera.catEyeRadius = flag(optCatEyeRadius);

  opts.autolook.enabled = bool(optAutolook);
  opts.autolook.azimuthDeg = flag(optAutolookAzimuth);
  opts.autolook.zenithDeg = float(optAutolookZenith);
  opts.autolook.margin = float(optAutolookMargin);
  opts.autolook.ignoreBackfaces = bool(optAutolookIgnoreBackfaces);

  // Parsed here so a typo fails before anything loads.
  opts.grid.range = parseWavelengthRangeFlag(std::string(optWavelengthRange));
  opts.grid.explicitWavelengths =
      parseWavelengthsFlag(std::string(optWavelengths));
  opts.grid.given = optWavelengthRange.getNumOccurrences() > 0 ||
                    optWavelengths.getNumOccurrences() > 0;
  opts.grid.jitter = bool(optWavelengthJitter);

  opts.sky.none = flag(optNoSunSky);
  opts.sky.sunZenithDeg = flag(optSunZenith);
  opts.sky.sunAzimuthDeg = flag(optSunAzimuth);
  opts.sky.visibility = flag(optSkyVisibility);
  opts.sky.waterVapor = flag(optSkyWaterVapor);
  opts.sky.scale = flag(optSkyScale);
  opts.sky.moonPhase = flag(optMoonPhase);
  opts.sky.moonDistance = flag(optMoonDistance);
  opts.sky.iblFileName = flag(optIBLFilename);
  opts.sky.iblScale = flag(optIBLScale);
  opts.sky.allLights = bool(optAllLights);
  opts.sky.noLightTree = bool(optNoLightTree);

  opts.haze.on = bool(optHaze);
  opts.haze.none = flag(optNoHaze);
  opts.haze.visibility = flag(optHazeVisibility);
  opts.haze.scaleHeight = flag(optHazeScaleHeight);

  // The manifold estimator, minus what needs a scene.
  opts.mneeEnabled = bool(optMNEE);
  opts.mneeReport = bool(optMNEEReport);
  opts.mneeSunOnly = bool(optMNEESunOnly);
  opts.mneeTestNormalHook = bool(optMNEETestNormalHook);
  opts.mnee.depth = optMNEE ? int(std::clamp(unsigned(optMNEEDepth), 1U,
                                             unsigned(MANIFOLD_MAX_DEPTH)))
                            : 0;
  opts.mnee.maxTrials = int(std::max(unsigned(optMNEEMaxTrials), 1U));
  opts.mnee.biasedTrials = int(unsigned(optMNEEBiased));
  opts.mnee.maxRoughness = std::max(float(optMNEEMaxRoughness), 0.0f);
  opts.mnee.minReceiverAlpha = std::max(float(optMNEEReceiverAlpha), 0.0f);

  // The default walk is terminated by Russian roulette, with the bounce
  // bound set high enough that clipping it is negligible even for
  // high-albedo transport; giving -max-bounces makes the bound the whole
  // termination rule, so the estimate is the fixed-depth truncation.
  opts.path.maxBounces = unsigned(optMaxBounces);
  opts.path.useRoulette = optMaxBounces.getNumOccurrences() == 0;
  opts.path.maxContribution = std::max(float(optMaxContribution), 0.0f);
  opts.path.maxContributionBounces =
      int(std::max(unsigned(optMaxContributionBounces), 1U));

  opts.tonemap.mode = parseAppearanceMode(std::string(optTonemap));
  opts.tonemap.curve = parseDisplayCurveKind(std::string(optCurve));
  opts.tonemap.local = parseLocalOperator(std::string(optLocal));
  opts.tonemap.exposure = float(optImageExposure);
  opts.tonemap.logDecades = float(optTonemapDecades);
  opts.tonemap.localStrength = float(optLocalStrength);
  opts.tonemap.localRange = float(optLocalRange);
  opts.tonemap.localClamp = float(optLocalClamp);
  // '-tonemap log' is kept as shorthand for '-tonemap linear -curve
  // log'; allow it, but not while the curve says otherwise.
  if (opts.tonemap.mode == AppearanceMode::LOG &&
      optCurve.getNumOccurrences() > 0 &&
      opts.tonemap.curve != DisplayCurveKind::LOG)
    throw smdl::Error("expected -curve log with -tonemap log ('-tonemap log' "
                      "is shorthand for '-tonemap linear -curve log')");

  opts.rgbPolicy.forceFalseColor =
      bool(optFalseColor) || optRGBWaves.getNumOccurrences() > 0;
  if (optRGBWaves.getNumOccurrences() > 0) {
    const auto waves{float3(optRGBWaves)};
    opts.rgbPolicy.falseColorWaves = {waves.x, waves.y, waves.z};
  }

  opts.progress.label = "Rendering";
  opts.progress.units = "px";
  opts.progress.style = parseProgressStyle(std::string(optProgress));
  opts.progress.filePath = std::string(optProgressFile);

  opts.output.rgb = std::string(optOutputRGB);
  opts.output.rgbFloat = std::string(optOutputRGBf);
  opts.output.spectrum = std::string(optOutputSpectrum);
  opts.output.spectrumGiven = optOutputSpectrum.getNumOccurrences() > 0;
  opts.output.resume = std::string(optResume);
  opts.output.previewEvery = double(optPreviewEvery);
  opts.output.profile = std::string(optProfile).empty()
                            ? std::string("smdl-toy.trace.json")
                            : std::string(optProfile);
  opts.output.profiling = optProfile.getNumOccurrences() > 0;

  // The command line as it was given, for the spectral output's
  // 'render args' field.
  for (int i = 1; i < argc; i++) {
    if (i > 1) opts.argsEcho += ' ';
    opts.argsEcho += argv[i];
  }
  return opts;
}
