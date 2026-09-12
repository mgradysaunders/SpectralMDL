#include "RenderFixtures.h"

#include <cmath>
#include <string>

#include "CameraModel.h"
#include "Options.h"
#include "Render/Sampler.h"

// The model is the one place the camera file, the body and the lens it
// names, and the few flags that frame and preview the picture meet. What
// matters is that the files speak for the instrument and the flags only
// where they may, that a body decides the pixels and the frame and the
// observer's frame follows the picture, that the preview keeps of the
// body what its exposure needs, that what has no meaning with the
// instrument is refused, and that the film quantity follows the sensor
// and nothing else.

namespace {

// The command line's own defaults, which `parseCommandLine()` supplies
// through `cl::init` and a hand-built `Options` has to supply itself.
[[nodiscard]] Options baseOptions() {
  Options opts{};
  opts.camera.lookFrom.value = float3(-6.0f, 0.0f, 2.0f);
  opts.camera.lookTo.value = float3(0.0f, 0.0f, 0.5f);
  opts.camera.lookUp.value = float3(0.0f, 0.0f, 1.0f);
  opts.image.resolution.value = int2(1280, 720);
  return opts;
}

constexpr const char *BODY = "sensor {\n"
                             "  name \"Test body\"\n"
                             "  pixels 600 400\n"
                             "  pitch 6\n"
                             "  response {\n"
                             "    band R { 400 1 700 1 }\n"
                             "    band G { 400 1 700 1 }\n"
                             "    band B { 400 1 700 1 }\n"
                             "    cfa { row R G  row G B }\n"
                             "  }\n"
                             "  detector { bits 14 }\n"
                             "  readout 0.03\n"
                             "  readout_direction up\n"
                             "}\n";

// The same body with a stated gain, so that its speed is fixed.
constexpr const char *FIXED_BODY = "sensor {\n"
                                   "  pixels 600 400\n"
                                   "  pitch 6\n"
                                   "  response { band L { 400 1 700 1 } }\n"
                                   "  detector { gain 0.5 }\n"
                                   "}\n";

// A body whose noise floor is its read noise alone, 4 e- under a 16384 e-
// well read out over 16 bits from a black level of 0: 12 stops.
constexpr const char *QUIET_BODY =
    "sensor {\n"
    "  pixels 600 400\n"
    "  pitch 6\n"
    "  response { band L { 400 1 700 1 } }\n"
    "  detector { full_well 16384 read_noise 4 dark_current 0 black_level 0 "
    "bits 16 }\n"
    "}\n";

// A frame of 120 by 80 mm, far past what the tube lens below covers.
constexpr const char *LARGE_BODY = "sensor {\n"
                                   "  pixels 600 400\n"
                                   "  pitch 200\n"
                                   "  response { band L { 400 1 700 1 } }\n"
                                   "}\n";

// A tiled body whose bands are zero for a way past their ends, and a
// fourth band, which the tile does not lay down, wider than the three.
constexpr const char *SHAPED_BODY =
    "sensor {\n"
    "  pixels 600 400\n"
    "  pitch 6\n"
    "  response {\n"
    "    band R { 500 0 560 0 580 1 680 1 700 0 780 0 }\n"
    "    band G { 460 0 480 1 600 1 620 0 }\n"
    "    band B { 300 0 360 0 380 0 400 1 500 1 520 0 }\n"
    "    band IR { 300 1 1000 1 }\n"
    "    cfa { row R G  row G B }\n"
    "  }\n"
    "}\n";

// A biconvex singlet with the stop against its back, as a file.
constexpr const char *SINGLET = "lens {\n"
                                "  surface { radius 50 thickness 4 ior 1.5 "
                                "diameter 20 }\n"
                                "  surface { radius -50 diameter 20 }\n"
                                "  stop { diameter 20 }\n"
                                "}\n";

// The same singlet in a catalog glass, which disperses.
constexpr const char *GLASS_SINGLET = "lens {\n"
                                      "  surface { radius 50 thickness 4 "
                                      "medium N-BK7 diameter 20 }\n"
                                      "  surface { radius -50 diameter 20 }\n"
                                      "  stop { diameter 20 }\n"
                                      "}\n";

// A singlet 100 mm behind a narrow stop, which lets it see a few degrees
// of field and no more: an image circle a few millimeters across.
constexpr const char *TUBE = "lens {\n"
                             "  stop { diameter 4 thickness 100 }\n"
                             "  surface { radius 50 thickness 4 ior 1.5 "
                             "diameter 10 }\n"
                             "  surface { radius -50 diameter 10 }\n"
                             "}\n";

// A scratch directory holding the body and the lens, and a camera file
// written per case.
class Files final {
public:
  explicit Files(const char *stem) : mTmpDir(stem) {
    (void)mTmpDir.write("body.sensor", BODY);
    (void)mTmpDir.write("fixed.sensor", FIXED_BODY);
    (void)mTmpDir.write("quiet.sensor", QUIET_BODY);
    (void)mTmpDir.write("large.sensor", LARGE_BODY);
    (void)mTmpDir.write("shaped.sensor", SHAPED_BODY);
    (void)mTmpDir.write("singlet.lens", SINGLET);
    (void)mTmpDir.write("glass.lens", GLASS_SINGLET);
    (void)mTmpDir.write("tube.lens", TUBE);
  }

  [[nodiscard]] Options camera(const std::string &text) {
    Options opts{baseOptions()};
    opts.camera.file = mTmpDir.write("shot.camera", text).string();
    return opts;
  }

  [[nodiscard]] std::string path(const char *name) const {
    return (mTmpDir / name).string();
  }

private:
  TempDir mTmpDir;
};

} // namespace

TEST_CASE("CameraModel: the observer's frame follows the picture") {
  ScopedShutter shutter{0.0f, 0.0f};
  Options opts{baseOptions()};
  SUBCASE("A 16:9 picture is 24 mm tall and as wide as it is for its "
          "height, with square pixels") {
    const CameraModel model{resolveCameraModel(opts)};
    CHECK(!model.hasPhysicalSensor());
    CHECK(model.filmQuantity() == FilmQuantity::RADIANCE);
    CHECK(model.resolution().x == 1280);
    CHECK(model.resolution().y == 720);
    CHECK(model.options.frameSize.y == 1e-3f * 24.0f);
    CHECK(model.options.frameSize.x ==
          doctest::Approx(1e-3f * 24.0f * 1280.0f / 720.0f));
    CHECK(model.cameraFileName.empty());
    CHECK(model.sensorFileName.empty());
    CHECK(model.lensFileName.empty());
  }
  SUBCASE("A 3:2 picture is full frame to the bit") {
    opts.image.resolution.value = int2(1200, 800);
    const CameraModel model{resolveCameraModel(opts)};
    CHECK(model.options.frameSize.x == 1e-3f * 36.0f);
    CHECK(model.options.frameSize.y == 1e-3f * 24.0f);
  }
  SUBCASE("The defaults, the file, and the framing flags win in that "
          "order") {
    Files files{"camera-model-order"};
    CHECK(resolveCameraModel(baseOptions()).options.fovYDeg == 37.8f);
    Options fromFile{files.camera("camera { fovy 30 look_from 1 2 3 }\n")};
    const CameraModel model{resolveCameraModel(fromFile)};
    CHECK(model.options.fovYDeg == 30.0f);
    CHECK(model.options.lookFrom.x == 1.0f);
    fromFile.camera.lookFrom = Flag<float3>{float3(4.0f, 5.0f, 6.0f), true};
    CHECK(resolveCameraModel(fromFile).options.lookFrom.x == 4.0f);
  }
}

TEST_CASE("CameraModel: a body decides the pixels and the frame") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-body"};
  Options opts{files.camera("camera { sensor \"body.sensor\" fstop 8 }\n")};
  SUBCASE("The pixels are the body's, the frame is the pitch over them, and "
          "the film holds irradiance") {
    const CameraModel model{resolveCameraModel(opts)};
    REQUIRE(model.hasPhysicalSensor());
    CHECK(model.sensor->name == "Test body");
    CHECK(model.filmQuantity() == FilmQuantity::IRRADIANCE);
    CHECK(model.resolution().x == 600);
    CHECK(model.resolution().y == 400);
    CHECK(model.options.frameSize.x == doctest::Approx(3.6e-3f));
    CHECK(model.options.frameSize.y == doctest::Approx(2.4e-3f));
    CHECK(model.sensorFileName == files.path("body.sensor"));
    CHECK(model.temperature == 25.0f);
  }
  SUBCASE("A -resolution that agrees is fine, and one that does not is "
          "refused") {
    opts.image.resolution = Flag<int2>{int2(600, 400), true};
    CHECK(resolveCameraModel(opts).resolution().x == 600);
    opts.image.resolution = Flag<int2>{int2(640, 480), true};
    CHECK_ERROR(
        smdl::catchAndReturnError([&] { (void)resolveCameraModel(opts); }),
        "is not the sensor's 600,400");
  }
  SUBCASE("The readout is the body's, then the camera file's") {
    (void)resolveCameraModel(opts);
    CHECK(gRenderShutter.readout == doctest::Approx(0.03f));
    CHECK(gRenderShutter.isReadoutReversed);
    const Options fromFile{
        files.camera("camera { sensor \"body.sensor\" fstop 8 "
                     "readout 0.01 readout_direction left }\n")};
    (void)resolveCameraModel(fromFile);
    CHECK(gRenderShutter.readout == doctest::Approx(0.01f));
    CHECK(gRenderShutter.isReadoutAlongX);
  }
  SUBCASE("The temperature reaches the model") {
    const Options warm{files.camera("camera { sensor \"body.sensor\" fstop 8 "
                                    "temperature 40 }\n")};
    CHECK(resolveCameraModel(warm).temperature == 40.0f);
  }
  SUBCASE("The report names the body") {
    const CameraModel model{resolveCameraModel(opts)};
    const std::string report{describeCamera(model)};
    CHECK_CONTAINS(report, "Test body");
    CHECK_CONTAINS(report, "spectral irradiance");
    CHECK_CONTAINS(report, "600 by 400 pixels at 6 um");
    CHECK_CONTAINS(report, "f/8");
  }
}

TEST_CASE("CameraModel: the stand-ins") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-stand-ins"};
  SUBCASE("A camera whose own sensor is human is the plain observer") {
    const CameraModel model{
        resolveCameraModel(files.camera("camera { sensor human }\n"))};
    CHECK(!model.hasPhysicalSensor());
    CHECK(model.resolution().x == 1280);
  }
  SUBCASE("A lens file is read and looked through") {
    const CameraModel model{
        resolveCameraModel(files.camera("camera { lens \"singlet.lens\" }\n"))};
    REQUIRE(model.options.lens);
    CHECK(model.options.lens->surfaces.size() == 3);
    CHECK(model.lensFileName == files.path("singlet.lens"));
    CHECK(!model.shouldApproximateLens());
  }
  SUBCASE("A camera whose own lens is ideal has nothing to fit") {
    Options opts{files.camera("camera { lens ideal }\n")};
    opts.camera.isIdeal = true;
    const CameraModel model{resolveCameraModel(opts)};
    CHECK(!model.options.lens);
    CHECK(!model.shouldApproximateLens());
  }
  SUBCASE("A lens on the observer keeps the observer's frame") {
    const CameraModel model{
        resolveCameraModel(files.camera("camera { lens \"singlet.lens\" }\n"))};
    CHECK(model.filmQuantity() == FilmQuantity::RADIANCE);
    CHECK(model.options.frameSize.y == 1e-3f * 24.0f);
  }
  SUBCASE("The report fits the thin lens to a lens whether or not it "
          "previews") {
    const std::string report{describeCamera(resolveCameraModel(
        files.camera("camera { lens \"singlet.lens\" }\n")))};
    CHECK_CONTAINS(report, "  ideal fit: the thin lens -ideal looks through, "
                           "a focal length of ");
  }
  SUBCASE("The report says how much of a frame a lens leaves dark") {
    const std::string report{describeCamera(resolveCameraModel(files.camera(
        "camera { sensor \"large.sensor\" lens \"tube.lens\" }\n")))};
    CHECK_CONTAINS(report, ", which reaches past it and leaves ");
    CHECK_CONTAINS(report, "% of the frame dark\n");
  }
}

TEST_CASE("CameraModel: what has no meaning with the instrument is refused") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-refusals"};
  const auto refused{[&](const Options &opts) {
    return smdl::catchAndReturnError([&] { (void)resolveCameraModel(opts); });
  }};
  SUBCASE("The thin lens's settings with a lens, the field of view first "
          "among them") {
    CHECK_ERROR(
        refused(files.camera("camera { lens \"singlet.lens\" fovy 30 }\n")),
        "\"fovy\" has no meaning with a lens");
    CHECK_ERROR(refused(files.camera("camera { lens \"singlet.lens\" "
                                     "vignetting 1 }\n")),
                "\"vignetting\" has no meaning with a lens");
    CHECK_ERROR(refused(files.camera("camera { lens \"singlet.lens\" "
                                     "focal_length 50 }\n")),
                "\"focal_length\" has no meaning with a lens");
  }
  SUBCASE("A body over a pinhole") {
    CHECK_ERROR(refused(files.camera("camera { sensor \"body.sensor\" }\n")),
                "a physical sensor integrates the irradiance over a pupil");
  }
  SUBCASE("A temperature for the observer") {
    CHECK_ERROR(refused(files.camera("camera { temperature 40 }\n")),
                "'temperature' is a physical sensor's condition");
  }
  SUBCASE("An ISO for the observer, from either source, in either form") {
    CHECK_ERROR(refused(files.camera("camera { iso 400 }\n")),
                "'iso' is a physical sensor's setting");
    CHECK_ERROR(refused(files.camera("camera { iso auto }\n")),
                "'iso' is a physical sensor's setting");
    Options flagged{files.camera("camera { }\n")};
    flagged.camera.iso = Flag<float>{400.0f, true};
    CHECK_ERROR(refused(flagged), "-iso is a physical sensor's setting");
  }
  SUBCASE("An ISO for a body whose gain is fixed") {
    CHECK_ERROR(refused(files.camera("camera { sensor \"fixed.sensor\" fstop 8 "
                                     "iso 400 }\n")),
                "'iso' has no meaning with a fixed gain");
    // The meter is the default, so asking for it contradicts nothing.
    CHECK_OK(refused(files.camera("camera { sensor \"fixed.sensor\" fstop 8 "
                                  "iso auto }\n")));
  }
  SUBCASE("A readout of the observer, or with no exposure") {
    Options human{files.camera("camera { }\n")};
    human.image.outputDN = "out-dn.img";
    CHECK_ERROR(refused(human), "-output-dn reads a body out");
    Options shut{files.camera("camera { sensor \"body.sensor\" fstop 8 }\n")};
    shut.image.outputDN = "out-dn.img";
    CHECK_ERROR(refused(shut), "-output-dn needs an exposure");
  }
  SUBCASE("The old response sidecar") {
    CHECK_ERROR(refused(files.camera("camera { sensor \"body.response\" }\n")),
                "names a '.response' file");
  }
  SUBCASE("Two spellings of the aperture") {
    CHECK_ERROR(refused(files.camera("camera { fstop 8 aperture 0.01 }\n")),
                "at most one of 'fstop' and 'aperture'");
  }
}

TEST_CASE("CameraModel: a refusal points at the key the file stated") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-caret"};
  const auto refused{[&](const Options &opts) {
    return smdl::catchAndReturnError([&] { (void)resolveCameraModel(opts); });
  }};
  SUBCASE("The message begins with the file, line, and column, and the "
          "excerpt marks the key") {
    const std::optional<smdl::Error> error{
        refused(files.camera("camera {\n  lens \"singlet.lens\"\n  fovy 30\n"
                             "}\n"))};
    REQUIRE(error);
    CHECK_CONTAINS(error->message, files.path("shot.camera") + ":3:3: ");
    CHECK_CONTAINS(error->message, "\"fovy\" has no meaning with a lens");
    CHECK_CONTAINS(error->snippet, "  fovy 30\n  ^~~~");
  }
  SUBCASE("The last statement of a key is the one marked") {
    const std::optional<smdl::Error> error{refused(files.camera(
        "camera { temperature 40 }\ncamera { temperature 41 }\n"))};
    REQUIRE(error);
    CHECK_CONTAINS(error->message, ":2:10: ");
    CHECK_CONTAINS(error->snippet, "temperature 41");
  }
  SUBCASE("A flag is named as a flag, with no excerpt") {
    Options flagged{files.camera("camera {\n  fovy 30\n}\n")};
    flagged.camera.iso = Flag<float>{400.0f, true};
    const std::optional<smdl::Error> error{refused(flagged)};
    REQUIRE(error);
    CHECK(error->message.rfind("-iso is a physical sensor's setting", 0) == 0);
    CHECK(error->snippet.empty());
  }
  SUBCASE("A setting stated only in a motion key has no key to mark") {
    const std::optional<smdl::Error> error{refused(files.camera(
        "camera {\n  lens \"singlet.lens\"\n  motion { at 0 fovy 30 }\n}\n"))};
    REQUIRE(error);
    CHECK(error->message.rfind("\"fovy\" has no meaning with a lens", 0) == 0);
    CHECK(error->snippet.empty());
  }
  SUBCASE("The pinhole over a body points at the body") {
    const std::optional<smdl::Error> error{
        refused(files.camera("camera {\n  sensor \"body.sensor\"\n}\n"))};
    REQUIRE(error);
    CHECK_CONTAINS(error->message, ":2:3: a physical sensor integrates");
    CHECK_CONTAINS(error->snippet, "^~~~~~");
  }
}

TEST_CASE("CameraModel: the thin lens's field, two ways") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-field"};
  const auto refused{[&](const Options &opts) {
    return smdl::catchAndReturnError([&] { (void)resolveCameraModel(opts); });
  }};
  SUBCASE("A focal length alone implies the field of view over 24 mm") {
    const CameraModel model{
        resolveCameraModel(files.camera("camera { focal_length "
                                        "50 }\n"))};
    CHECK(model.options.frameSize.y == 1e-3f * 24.0f);
    CHECK(model.options.fovYDeg ==
          doctest::Approx(2 * smdl::degrees(std::atan(12.0f / 50.0f))));
    CHECK(thinLensFocalLength(model.options) == doctest::Approx(0.05f));
  }
  SUBCASE("Both together size the observer's frame") {
    const CameraModel model{resolveCameraModel(
        files.camera("camera { fovy 30 focal_length 50 }\n"))};
    CHECK(model.options.fovYDeg == 30.0f);
    CHECK(model.options.frameSize.y ==
          doctest::Approx(2 * 0.05f * std::tan(smdl::radians(15.0f))));
    CHECK(model.options.frameSize.x ==
          doctest::Approx(model.options.frameSize.y * 1280.0f / 720.0f));
    CHECK(thinLensFocalLength(model.options) == doctest::Approx(0.05f));
    CHECK(!model.hasPhysicalSensor());
  }
  SUBCASE("Neither leaves the default field over 24 mm") {
    const CameraModel model{resolveCameraModel(files.camera("camera { }\n"))};
    CHECK(model.options.fovYDeg == 37.8f);
    CHECK(model.options.frameSize.y == 1e-3f * 24.0f);
  }
  SUBCASE("Over a body the focal length states the field over the body's "
          "frame") {
    const CameraModel model{resolveCameraModel(files.camera(
        "camera { sensor \"body.sensor\" focal_length 4.8 fstop 2 }\n"))};
    // A 2.4 mm frame under a 4.8 mm lens: the half height is a quarter
    // of the focal length.
    CHECK(model.options.frameSize.y == doctest::Approx(2.4e-3f));
    CHECK(model.options.fovYDeg ==
          doctest::Approx(2 * smdl::degrees(std::atan(0.25f))));
    CHECK(thinLensFocalLength(model.options) == doctest::Approx(4.8e-3f));
  }
  SUBCASE("Over a body the field of view alone states it too, and both "
          "are refused") {
    const CameraModel model{resolveCameraModel(
        files.camera("camera { sensor \"body.sensor\" fovy 30 fstop 2 }\n"))};
    CHECK(model.options.fovYDeg == 30.0f);
    CHECK(model.options.frameSize.y == doctest::Approx(2.4e-3f));
    CHECK_ERROR(refused(files.camera("camera { sensor \"body.sensor\" fovy 30 "
                                     "focal_length 5 fstop 2 }\n")),
                "'fovy' and 'focal_length' are two statements");
  }
  SUBCASE("The preview of a body follows the body's rule") {
    Options opts{
        files.camera("camera { sensor \"body.sensor\" focal_length 4.8 "
                     "fstop 2 }\n")};
    opts.camera.isIdeal = true;
    const CameraModel model{resolveCameraModel(opts)};
    CHECK(!model.hasPhysicalSensor());
    CHECK(model.options.frameSize.y == doctest::Approx(2.4e-3f));
    CHECK(model.options.fovYDeg ==
          doctest::Approx(2 * smdl::degrees(std::atan(0.25f))));
  }
}

TEST_CASE("CameraModel: the ISO") {
  ScopedShutter shutter{0.0f, 0.004f};
  Files files{"camera-model-iso"};
  SUBCASE("Stated in the file, by the flag, or metered when neither says") {
    const CameraModel stated{resolveCameraModel(
        files.camera("camera { sensor \"body.sensor\" fstop 8 iso 400 }\n"))};
    REQUIRE(stated.iso);
    CHECK(*stated.iso == 400.0f);
    Options flagged{
        files.camera("camera { sensor \"body.sensor\" fstop 8 iso 400 }\n")};
    flagged.camera.iso = Flag<float>{800.0f, true};
    REQUIRE(resolveCameraModel(flagged).iso);
    CHECK(*resolveCameraModel(flagged).iso == 800.0f);
    flagged.camera.iso = Flag<float>{};
    flagged.camera.shouldMeterISO = true;
    CHECK(!resolveCameraModel(flagged).iso);
    CHECK(!resolveCameraModel(
               files.camera("camera { sensor \"body.sensor\" fstop 8 }\n"))
               .iso);
  }
  SUBCASE("The report states the well, the base ISO, and the ISO") {
    const CameraModel metered{resolveCameraModel(
        files.camera("camera { sensor \"body.sensor\" fstop 8 }\n"))};
    const std::string report{describeCamera(metered)};
    CHECK_CONTAINS(report, "  well: 36000 e- from the pitch, the generic "
                           "well; base ISO ");
    CHECK_CONTAINS(report, " from the well, \"R\" counting ");
    CHECK_CONTAINS(report, " e- per lux-second under D55\n");
    CHECK_CONTAINS(report, "  iso: auto, metered from the film once it is "
                           "rendered, from the base ");
    const CameraModel stated{resolveCameraModel(
        files.camera("camera { sensor \"body.sensor\" fstop 8 iso 800 }\n"))};
    CHECK_CONTAINS(describeCamera(stated), "  iso: 800 stated, ");
    CHECK_CONTAINS(describeCamera(stated), " DN/e-\n");
    const CameraModel fixed{resolveCameraModel(
        files.camera("camera { sensor \"fixed.sensor\" fstop 8 }\n"))};
    CHECK_CONTAINS(describeCamera(fixed),
                   ", the saturation speed of the stated gain, so nothing "
                   "is metered\n");
    CHECK_CONTAINS(describeCamera(fixed), " a stated gain of 0.5 DN/e-\n");
  }
  SUBCASE("The report states the exposure in a photographer's terms") {
    // f/8 at 1/250 s is EV 14, two stops of which ISO 400 spends, and the
    // meter's `N^2 / t = L S / K` makes it 12.5 * 64 / (0.004 * 400) = 500
    // cd/m^2 on average.
    const std::string stated{describeCamera(resolveCameraModel(files.camera(
        "camera { sensor \"body.sensor\" fstop 8 shutter 0.004 iso 400 }\n")))};
    CHECK_CONTAINS(stated, "  exposure: EV 14 (f/8 at 1/250 s): at ISO 400 it "
                           "suits EV100 12, about overcast, a mean scene "
                           "luminance of 500 cd/m^2\n");
    const std::string metered{describeCamera(resolveCameraModel(files.camera(
        "camera { sensor \"body.sensor\" fstop 8 shutter 0.004 }\n")))};
    CHECK_CONTAINS(metered, "  exposure: EV 14 (f/8 at 1/250 s): the meter's "
                            "ISO fits it to scenes from EV100 ");
    CHECK_CONTAINS(metered, " at ISO 102400, about ");
  }
  SUBCASE("The report states the dynamic range, at the base until an ISO is "
          "chosen") {
    const std::string base{describeCamera(resolveCameraModel(
        files.camera("camera { sensor \"quiet.sensor\" fstop 8 }\n")))};
    CHECK_CONTAINS(base, "  dynamic range: 12 stops at the base ISO ");
    CHECK_CONTAINS(base, ": 16384 e- over a floor of 4 e- of read, dark, and "
                         "quantization noise, less by about a stop for each "
                         "stop the meter goes above the base\n");
    const std::string stated{describeCamera(resolveCameraModel(
        files.camera("camera { sensor \"quiet.sensor\" fstop 8 iso 800 }\n")))};
    CHECK_CONTAINS(stated, " stops at ISO 800: ");
  }
}

TEST_CASE("CameraModel: the focus") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-focus"};
  SUBCASE("A distance, infinity, or the autofocus, from the file") {
    CHECK(resolveCameraModel(files.camera("camera { focus 4 }\n"))
              .options.focus == 4.0f);
    const CameraModel atInfinity{
        resolveCameraModel(files.camera("camera { focus infinity }\n"))};
    CHECK(std::isinf(atInfinity.options.focus));
    CHECK(!atInfinity.shouldAutofocus);
    const CameraModel automatic{
        resolveCameraModel(files.camera("camera { focus auto }\n"))};
    CHECK(automatic.shouldAutofocus);
    CHECK(automatic.options.focus == 0.0f);
  }
  SUBCASE("The report states the focus and the depth of field") {
    const std::string report{describeCamera(resolveCameraModel(
        files.camera("camera { focal_length 50 fstop 8 focus 5 }\n")))};
    CHECK_CONTAINS(report, "focus: 5 scene units");
    // 50 mm at f/8 focused at 5 m on a 42.67 by 24 mm frame, whose
    // circle of confusion is 0.0326 mm: hyperfocal at 9.63 m, sharp
    // from 3.30 to 10.35 m.
    CHECK_CONTAINS(report, "depth of field: 3.29");
    CHECK_CONTAINS(report, " to 10.3");
    CHECK_CONTAINS(report, "hyperfocal 9.62");
    CHECK_CONTAINS(report, "circle of confusion of 0.0326");
    const std::string automatic{describeCamera(
        resolveCameraModel(files.camera("camera { fstop 8 focus auto }\n")))};
    CHECK_CONTAINS(automatic, "focus: auto, measured from the scene");
    CHECK(automatic.find("depth of field") == std::string::npos);
    const std::string pinhole{
        describeCamera(resolveCameraModel(files.camera("camera { }\n")))};
    CHECK_CONTAINS(pinhole, "a pinhole, so everything is in focus");
  }
}

TEST_CASE("CameraModel: the white balance") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-white-balance"};
  const auto refused{[&](const Options &opts) {
    return smdl::catchAndReturnError([&] { (void)resolveCameraModel(opts); });
  }};
  SUBCASE("D65 unless stated, the file's, and the flag's over it") {
    CHECK(resolveCameraModel(
              files.camera("camera { sensor \"body.sensor\" fstop 8 }\n"))
              .whiteBalance.kind == WhiteBalanceKind::D65);
    Options opts{files.camera(
        "camera { sensor \"body.sensor\" fstop 8 white_balance tungsten }\n")};
    CHECK(resolveCameraModel(opts).whiteBalance.kind ==
          WhiteBalanceKind::TUNGSTEN);
    opts.camera.whiteBalance = Flag<WhiteBalance>{
        WhiteBalance{WhiteBalanceKind::KELVIN, 4300.0f}, true};
    const CameraModel model{resolveCameraModel(opts)};
    CHECK(model.whiteBalance.kind == WhiteBalanceKind::KELVIN);
    CHECK(model.whiteBalance.kelvin == 4300.0f);
  }
  SUBCASE("Stated for the observer it is refused, pointed at the key") {
    const std::optional<smdl::Error> error{
        refused(files.camera("camera {\n  white_balance auto\n}\n"))};
    CHECK_ERROR(error, ":2:3: 'white_balance' is a physical sensor's setting");
    Options flagged{files.camera("camera { fovy 30 }\n")};
    flagged.camera.whiteBalance = Flag<WhiteBalance>{WhiteBalance{}, true};
    CHECK_ERROR(refused(flagged),
                "-white-balance is a physical sensor's setting");
  }
  SUBCASE("The report states the fit and the white balance, and bands too "
          "much alike to fit") {
    const std::string report{describeCamera(
        resolveCameraModel(files.camera("camera { sensor \"body.sensor\" fstop "
                                        "8 white_balance daylight }\n")))};
    CHECK_CONTAINS(report, "  color: R, G, and B respond too much alike to "
                           "tell colors apart, so the develop is false "
                           "color; white balance daylight\n");
  }
}

TEST_CASE("CameraModel: what only the observer's develop does is refused "
          "with a body") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-develop"};
  const auto refused{[&](const Options &opts) {
    return smdl::catchAndReturnError([&] { (void)resolveCameraModel(opts); });
  }};
  Options opts{files.camera("camera { sensor \"body.sensor\" fstop 8 }\n")};
  SUBCASE("The night tonemap") {
    opts.image.tonemap.isNight = true;
    CHECK_ERROR(refused(opts), "-tonemap night models the observer's eyes");
  }
  SUBCASE("False color, in either spelling") {
    opts.image.rgbPolicy.shouldForceFalseColor = true;
    CHECK_ERROR(refused(opts), "-false-color maps the spectral film's bands");
    opts.image.rgbPolicy.falseColorWaves = {650.0f, 550.0f, 450.0f};
    CHECK_ERROR(refused(opts),
                "-rgb-wavelengths maps the spectral film's bands");
  }
  SUBCASE("Both are the observer's to have") {
    Options observer{baseOptions()};
    observer.image.tonemap.isNight = true;
    observer.image.rgbPolicy.shouldForceFalseColor = true;
    CHECK_OK(refused(observer));
  }
}

TEST_CASE("CameraModel: a render of a physical sensor needs an exposure") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-render"};
  const auto unrenderable{[&](const Options &opts) {
    return smdl::catchAndReturnError(
        [&] { refuseUnrenderable(resolveCameraModel(opts)); });
  }};
  SUBCASE("A body with the shutter shut is refused, pointed at the body") {
    const std::optional<smdl::Error> error{unrenderable(
        files.camera("camera {\n  sensor \"body.sensor\"\n  fstop 8\n}\n"))};
    CHECK_ERROR(error, files.path("shot.camera") + ":2:3: a physical sensor "
                                                   "counts the electrons of "
                                                   "an exposure");
  }
  SUBCASE("A shutter, or the observer, renders") {
    CHECK_OK(unrenderable(files.camera(
        "camera { sensor \"body.sensor\" fstop 8 shutter 0.01 }\n")));
    CHECK_OK(unrenderable(baseOptions()));
  }
  SUBCASE("The report says so where it states the shutter") {
    CHECK_CONTAINS(
        describeCamera(resolveCameraModel(
            files.camera("camera { sensor \"body.sensor\" fstop 8 }\n"))),
        "; a physical sensor cannot render with a shut shutter: state "
        "'shutter'\n");
  }
}

TEST_CASE("CameraModel: the preview") {
  ScopedShutter shutter{0.0f, 0.01f};
  Files files{"camera-model-preview"};
  const auto preview{[&](const std::string &text) {
    Options opts{files.camera(text)};
    opts.camera.isIdeal = true;
    return opts;
  }};
  SUBCASE("-ideal puts the observer on the body's frame and pixels, and "
          "fits the thin lens to the lens") {
    const CameraModel model{resolveCameraModel(
        preview("camera { sensor \"body.sensor\" lens \"singlet.lens\" }\n"))};
    CHECK(model.isPreview);
    CHECK(!model.hasPhysicalSensor());
    REQUIRE(model.previewedSensor);
    CHECK(model.previewedSensor->name == "Test body");
    CHECK(model.sensorFileName == files.path("body.sensor"));
    CHECK(model.shouldApproximateLens());
    CHECK(model.filmQuantity() == FilmQuantity::RADIANCE);
    CHECK(model.resolution().x == 600);
    CHECK(model.options.frameSize.x == doctest::Approx(3.6e-3f));
  }
  SUBCASE("The ISO stays for the exposure, and what only the develop and "
          "the noise use goes") {
    const CameraModel model{
        resolveCameraModel(preview("camera { sensor \"body.sensor\" fstop 8 "
                                   "iso 400 white_balance shade temperature "
                                   "40 }\n"))};
    REQUIRE(model.iso);
    CHECK(*model.iso == 400.0f);
    CHECK(model.whiteBalance.kind == WhiteBalanceKind::D65);
    CHECK(model.temperature == 25.0f);
  }
  SUBCASE("A camera whose sensor is human has nothing to preview") {
    const CameraModel model{
        resolveCameraModel(preview("camera { fovy 30 }\n"))};
    CHECK(model.isPreview);
    CHECK(!model.previewedSensor);
    CHECK(!model.shouldApproximateLens());
  }
  SUBCASE("A readout has no body to read") {
    Options opts{preview("camera { sensor \"body.sensor\" fstop 8 }\n")};
    opts.image.outputDN = files.path("shot.img");
    CHECK_ERROR(
        smdl::catchAndReturnError([&] { (void)resolveCameraModel(opts); }),
        "-output-dn reads a body out, and -ideal previews it");
  }
  SUBCASE("Through the thin lens, the body's film would hold its pupil "
          "integral on axis") {
    // At f/8 the aperture's radius is a sixteenth of the focal length, and
    // focused at infinity the pupil integral is pi R^2 / (R^2 + f^2).
    CameraModel model{resolveCameraModel(
        preview("camera { sensor \"body.sensor\" fstop 8 focus infinity }\n"))};
    (void)buildCamera(model);
    CHECK(model.previewIrradianceScale ==
          doctest::Approx(3.14159265358979 / 257).epsilon(1e-5));
  }
  SUBCASE("Through a lens, what the lens puts on the middle of a body's "
          "film") {
    CameraModel model{resolveCameraModel(
        preview("camera { sensor \"body.sensor\" lens \"singlet.lens\" focus "
                "infinity }\n"))};
    CameraOptions irradiance{model.options};
    irradiance.filmQuantity = FilmQuantity::IRRADIANCE;
    const Camera traced{irradiance};
    constexpr uint32_t NUM_SAMPLES = 256;
    double total{};
    for (uint32_t i = 0; i < NUM_SAMPLES; i++) {
      Sampler sampler{};
      sampler.startPixelSample(uint32_t(200 * 600 + 300), i);
      total += double(traced.sample(300, 200, sampler).weight);
    }
    (void)buildCamera(model);
    CHECK(model.previewIrradianceScale ==
          doctest::Approx(total / NUM_SAMPLES).epsilon(0.02));
  }
  SUBCASE("A shut shutter leaves no exposure to simulate") {
    ScopedShutter shut{0.0f, 0.0f};
    Options opts{preview("camera { sensor \"body.sensor\" fstop 8 }\n")};
    CHECK_ERROR(smdl::catchAndReturnError(
                    [&] { refuseUnrenderable(resolveCameraModel(opts)); }),
                "-ideal exposes the picture as the body would");
  }
  SUBCASE("The report says what the preview stands in for") {
    const std::string report{describeCamera(resolveCameraModel(
        preview("camera { sensor \"body.sensor\" lens \"singlet.lens\" }\n")))};
    CHECK_CONTAINS(report, ", previewed with -ideal\n");
    CHECK_CONTAINS(report, "  ideal fit: the thin lens -ideal looks through, "
                           "a focal length of ");
    CHECK_CONTAINS(report, "sensor: the observer, previewing \"Test body\" ");
  }
}

TEST_CASE("CameraModel: the resolution scale") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-resolution-scale"};
  SUBCASE("A body renders exactly its own pixels, so it is refused") {
    Options opts{files.camera("camera { sensor \"body.sensor\" fstop 8 }\n")};
    opts.image.resolutionScale = Flag<float>{0.25f, true};
    CHECK_ERROR(
        smdl::catchAndReturnError([&] { (void)resolveCameraModel(opts); }),
        "-resolution-scale renders a smaller picture of the frame, "
        "and a body renders exactly its own pixels");
  }
  SUBCASE("Under -ideal the body's frame keeps its size over fewer pixels") {
    Options opts{files.camera("camera { sensor \"body.sensor\" fstop 8 }\n")};
    opts.camera.isIdeal = true;
    opts.image.resolutionScale = Flag<float>{0.25f, true};
    const CameraModel model{resolveCameraModel(opts)};
    CHECK(model.resolution().x == 150);
    CHECK(model.resolution().y == 100);
    CHECK(model.options.frameSize.x == doctest::Approx(3.6e-3f));
  }
  SUBCASE("For the observer it scales -resolution over the same frame") {
    Options opts{baseOptions()};
    opts.image.resolutionScale = Flag<float>{0.5f, true};
    const CameraModel model{resolveCameraModel(opts)};
    CHECK(model.resolution().x == 640);
    CHECK(model.resolution().y == 360);
    CHECK(model.options.frameSize.y == 1e-3f * 24.0f);
    CHECK(model.options.frameSize.x ==
          doctest::Approx(1e-3f * 24.0f * 1280.0f / 720.0f));
  }
}

TEST_CASE("CameraModel: the wavelengths a dispersive lens is bounded over") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-trace-range"};
  SUBCASE("A tiled body and a lens whose glasses disperse set the span of "
          "the bands the tile lays down") {
    const CameraModel model{resolveCameraModel(files.camera(
        "camera { sensor \"shaped.sensor\" lens \"glass.lens\" }\n"))};
    // B is zero up to 380 nm and R from 700 nm on, whatever knots are
    // stated past them, and the band the tile leaves out reaches past
    // both.
    REQUIRE(model.options.traceWavelengthRange);
    CHECK(model.options.traceWavelengthRange->x == 380.0f);
    CHECK(model.options.traceWavelengthRange->y == 700.0f);
  }
  SUBCASE("A lens whose glasses do not disperse sets none") {
    const CameraModel model{resolveCameraModel(files.camera(
        "camera { sensor \"shaped.sensor\" lens \"singlet.lens\" }\n"))};
    CHECK(!model.options.traceWavelengthRange);
  }
  SUBCASE("Nor does a body without a tile, the observer, or the preview") {
    CHECK(!resolveCameraModel(
               files.camera(
                   "camera { sensor \"quiet.sensor\" lens \"glass.lens\" }\n"))
               .options.traceWavelengthRange);
    CHECK(!resolveCameraModel(files.camera("camera { lens \"glass.lens\" }\n"))
               .options.traceWavelengthRange);
    Options preview{files.camera(
        "camera { sensor \"shaped.sensor\" lens \"glass.lens\" }\n")};
    preview.camera.isIdeal = true;
    CHECK(!resolveCameraModel(preview).options.traceWavelengthRange);
  }
  SUBCASE("The report states the media, the color, and what each band "
          "traces the lens at") {
    const std::string report{describeCamera(resolveCameraModel(files.camera(
        "camera { sensor \"shaped.sensor\" lens \"glass.lens\" }\n")))};
    CHECK_CONTAINS(report, "  after surface 1: \"N-BK7\", nd 1.5168, Vd 64.17");
    CHECK_CONTAINS(report, "  color: the F line focuses ");
    // Over the band's own curve, which R is zero outside of from 560 to
    // 700 nm.
    CHECK_CONTAINS(report, "  traced: at a wavelength each pixel draws from "
                           "its band: \"R\" 560-700 nm, median ");
  }
  SUBCASE("Without a tile the report says the d line, and under the preview "
          "nothing") {
    CHECK_CONTAINS(describeCamera(resolveCameraModel(
                       files.camera("camera { lens \"glass.lens\" }\n"))),
                   "  traced: at the d line (588 nm) alone");
    Options preview{files.camera(
        "camera { sensor \"shaped.sensor\" lens \"glass.lens\" }\n")};
    preview.camera.isIdeal = true;
    CHECK_CONTAINS(describeCamera(resolveCameraModel(preview)),
                   "  traced: not at all");
  }
}
