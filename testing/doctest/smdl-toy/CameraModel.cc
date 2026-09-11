#include "RenderFixtures.h"

#include <cmath>
#include <string>

#include "CameraModel.h"
#include "Options.h"

// The model is the one place the camera file, the body and the lens it
// names, and the command line meet. What matters is the order they win
// in, that a body decides the pixels and the frame and the observer's
// frame follows the picture, that each stand-in keeps what the plan says
// it keeps, that what has no meaning with the instrument is refused, and
// that the film quantity follows the sensor and nothing else.

namespace {

// The command line's own defaults, which `parseCommandLine()` supplies
// through `cl::init` and a hand-built `Options` has to supply itself.
[[nodiscard]] Options baseOptions() {
  auto opts{Options{}};
  opts.camera.lookFrom.value = float3(-6.0f, 0.0f, 2.0f);
  opts.camera.lookTo.value = float3(0.0f, 0.0f, 0.5f);
  opts.camera.lookUp.value = float3(0.0f, 0.0f, 1.0f);
  opts.camera.fovYDeg.value = 37.8f;
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

// A biconvex singlet with the stop against its back, as a file.
constexpr const char *SINGLET = "lens {\n"
                                "  surface { radius 50 thickness 4 ior 1.5 "
                                "diameter 20 }\n"
                                "  surface { radius -50 diameter 20 }\n"
                                "  stop { diameter 20 }\n"
                                "}\n";

// A scratch directory holding the body and the lens, and a camera file
// written per case.
class Files final {
public:
  explicit Files(const char *stem) : mTmpDir(stem) {
    (void)mTmpDir.write("body.sensor", BODY);
    (void)mTmpDir.write("singlet.lens", SINGLET);
  }

  [[nodiscard]] Options camera(const std::string &text) {
    auto opts{baseOptions()};
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
  auto opts{baseOptions()};
  SUBCASE("A 16:9 picture is 24 mm tall and as wide as it is for its "
          "height, with square pixels") {
    const auto model{resolveCameraModel(opts)};
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
    const auto model{resolveCameraModel(opts)};
    CHECK(model.options.frameSize.x == 1e-3f * 36.0f);
    CHECK(model.options.frameSize.y == 1e-3f * 24.0f);
  }
  SUBCASE("The defaults, the file, and the flags win in that order") {
    Files files{"camera-model-order"};
    CHECK(resolveCameraModel(baseOptions()).options.fovYDeg == 37.8f);
    auto fromFile{files.camera("camera { fovy 30 }\n")};
    CHECK(resolveCameraModel(fromFile).options.fovYDeg == 30.0f);
    fromFile.camera.fovYDeg = Flag<float>{50.0f, true};
    CHECK(resolveCameraModel(fromFile).options.fovYDeg == 50.0f);
  }
}

TEST_CASE("CameraModel: a body decides the pixels and the frame") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-body"};
  auto opts{files.camera("camera { sensor \"body.sensor\" fstop 8 }\n")};
  SUBCASE("The pixels are the body's, the frame is the pitch over them, and "
          "the film holds irradiance") {
    const auto model{resolveCameraModel(opts)};
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
  SUBCASE("The readout is the body's, then the camera file's, then the "
          "flag's") {
    (void)resolveCameraModel(opts);
    CHECK(gRenderShutter.readout == doctest::Approx(0.03f));
    CHECK(gRenderShutter.isReadoutReversed);
    auto fromFile{files.camera("camera { sensor \"body.sensor\" fstop 8 "
                               "readout 0.01 readout_direction left }\n")};
    (void)resolveCameraModel(fromFile);
    CHECK(gRenderShutter.readout == doctest::Approx(0.01f));
    CHECK(gRenderShutter.isReadoutAlongX);
    fromFile.camera.readout = Flag<float>{0.002f, true};
    (void)resolveCameraModel(fromFile);
    CHECK(gRenderShutter.readout == doctest::Approx(0.002f));
  }
  SUBCASE("The temperature reaches the model") {
    const auto warm{files.camera("camera { sensor \"body.sensor\" fstop 8 "
                                 "temperature 40 }\n")};
    CHECK(resolveCameraModel(warm).temperature == 40.0f);
  }
  SUBCASE("The report names the body") {
    const auto model{resolveCameraModel(opts)};
    const auto report{describeCamera(model)};
    CHECK_CONTAINS(report, "Test body");
    CHECK_CONTAINS(report, "spectral irradiance");
    CHECK_CONTAINS(report, "600 by 400 pixels at 6 um");
    CHECK_CONTAINS(report, "f/8");
  }
}

TEST_CASE("CameraModel: the stand-ins") {
  ScopedShutter shutter{0.0f, 0.0f};
  Files files{"camera-model-stand-ins"};
  SUBCASE("-sensor human replaces a body with the observer on the body's "
          "frame and pixels") {
    auto opts{files.camera("camera { sensor \"body.sensor\" fstop 8 "
                           "temperature 40 }\n")};
    opts.camera.sensor = Flag<std::string>{"human", true};
    const auto model{resolveCameraModel(opts)};
    CHECK(!model.hasPhysicalSensor());
    CHECK(model.filmQuantity() == FilmQuantity::RADIANCE);
    CHECK(model.resolution().x == 600);
    CHECK(model.options.frameSize.x == doctest::Approx(3.6e-3f));
    CHECK(model.sensorFileName.empty());
    CHECK_CONTAINS(describeCamera(model), "the observer");
  }
  SUBCASE("A camera whose own sensor is human is the plain observer") {
    const auto model{
        resolveCameraModel(files.camera("camera { sensor human }\n"))};
    CHECK(!model.hasPhysicalSensor());
    CHECK(model.resolution().x == 1280);
  }
  SUBCASE("A lens file is read, and -lens ideal puts the thin lens back") {
    auto opts{files.camera("camera { lens \"singlet.lens\" }\n")};
    const auto lensed{resolveCameraModel(opts)};
    REQUIRE(lensed.options.lens);
    CHECK(lensed.options.lens->surfaces.size() == 3);
    CHECK(lensed.lensFileName == files.path("singlet.lens"));
    opts.camera.lens = Flag<std::string>{"ideal", true};
    const auto ideal{resolveCameraModel(opts)};
    CHECK(!ideal.options.lens);
    CHECK(ideal.lensFileName.empty());
  }
  SUBCASE("A lens on the observer keeps the observer's frame") {
    const auto model{
        resolveCameraModel(files.camera("camera { lens \"singlet.lens\" }\n"))};
    CHECK(model.filmQuantity() == FilmQuantity::RADIANCE);
    CHECK(model.options.frameSize.y == 1e-3f * 24.0f);
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
        "'fovy' has no meaning with a lens");
    CHECK_ERROR(refused(files.camera("camera { lens \"singlet.lens\" "
                                     "vignetting 1 }\n")),
                "'vignetting' has no meaning with a lens");
    auto flagged{files.camera("camera { lens \"singlet.lens\" }\n")};
    flagged.camera.fovYDeg = Flag<float>{30.0f, true};
    CHECK_ERROR(refused(flagged), "'fovy' has no meaning with a lens");
  }
  SUBCASE("A body over a pinhole") {
    CHECK_ERROR(refused(files.camera("camera { sensor \"body.sensor\" }\n")),
                "a physical sensor integrates the irradiance over a pupil");
  }
  SUBCASE("A temperature for the observer") {
    CHECK_ERROR(refused(files.camera("camera { temperature 40 }\n")),
                "'temperature' is a physical sensor's condition");
  }
  SUBCASE("A readout of the observer, or with no exposure") {
    auto human{files.camera("camera { }\n")};
    human.image.outputDN = "out-dn.img";
    CHECK_ERROR(refused(human), "-output-dn reads a body out");
    auto shut{files.camera("camera { sensor \"body.sensor\" fstop 8 }\n")};
    shut.image.outputDN = "out-dn.img";
    CHECK_ERROR(refused(shut), "-output-dn needs an exposure");
  }
  SUBCASE("The old response sidecar, from either source") {
    CHECK_ERROR(refused(files.camera("camera { sensor \"body.response\" }\n")),
                "names a '.response' file");
    auto flagged{baseOptions()};
    flagged.camera.sensor = Flag<std::string>{"body.response", true};
    CHECK_ERROR(refused(flagged), "names a '.response' file");
  }
  SUBCASE("Two spellings of the aperture") {
    CHECK_ERROR(refused(files.camera("camera { fstop 8 aperture 0.01 }\n")),
                "at most one of -fstop and -aperture");
  }
}
