#include "Fixtures.h"

#include <cmath>
#include <filesystem>
#include <string>
#include <utility>

#include "Layout/SensorFile.h"

namespace {
// Parse from memory and require no errors.
SensorDocument parseOK(LayoutDiagnostics &diags, std::string text) {
  const auto &source{diags.addSource("test.sensor", std::move(text))};
  auto document{parseSensor(diags, source)};
  if (diags.hasErrors()) MESSAGE(diags.renderAll(false));
  REQUIRE(!diags.hasErrors());
  return document;
}

// Parse from memory over a sink of its own, require exactly one error,
// and hand back it back so the caller can say which one it expected.
// The sink is local so that a subcase can ask several times.
LayoutDiagnostic parseError(std::string text) {
  LayoutDiagnostics diags{};
  const auto &source{diags.addSource("test.sensor", std::move(text))};
  (void)parseSensor(diags, source);
  REQUIRE(diags.errorCount() == 1);
  return diags.all().front();
}

// The shortest thing that is a body: the pixels, the pitch, and one
// band of two knots. The base every malformed case below perturbs.
constexpr const char *BASE = "sensor {\n"
                             "  pixels 4 3  pitch 2.5\n"
                             "  response { band vis { 400 1 700 1 } }\n"
                             "}\n";

// The base with `setting` added inside the sensor block.
std::string sensorWith(const char *setting) {
  return std::string("sensor { pixels 4 3 pitch 2.5 response { band vis { "
                     "400 1 700 1 } } ") +
         setting + " }\n";
}

// The base with `setting` added inside the response block.
std::string responseWith(const char *setting) {
  return std::string("sensor { pixels 4 3 pitch 2.5 response { band vis { "
                     "400 1 700 1 } ") +
         setting + " } }\n";
}

// The base with `setting` inside a detector block.
std::string detectorWith(const char *setting) {
  return std::string("sensor { pixels 4 3 pitch 2.5 response { band vis { "
                     "400 1 700 1 } } detector { ") +
         setting + " } }\n";
}
} // namespace

TEST_CASE("SensorFile: the body's geometry") {
  LayoutDiagnostics diags{};
  SUBCASE("The base parses: pixels, one pitch for square pixels, and the "
          "frame follows") {
    const auto document{parseOK(diags, BASE)};
    const auto &sensor{document.sensor};
    CHECK(sensor.pixels.x == 4);
    CHECK(sensor.pixels.y == 3);
    CHECK(sensor.pitchUM.x == 2.5f);
    CHECK(sensor.pitchUM.y == 2.5f);
    CHECK(sensor.sizeMM().x == doctest::Approx(0.01));
    CHECK(sensor.sizeMM().y == doctest::Approx(0.0075));
    CHECK(sensor.pixelArea() == doctest::Approx(6.25e-12));
    CHECK(sensor.name.empty());
    CHECK(!sensor.hasDetectorBlock);
    CHECK(sensor.readout == 0.0f);
    CHECK(sensor.readoutDirection == ReadoutDirection::DOWN);
    REQUIRE(document.sensorLoc);
    CHECK(document.source->lineAndColumn(document.sensorLoc.offset).lineNo ==
          1);
  }
  SUBCASE("Two pitches are the rare rectangular pixel") {
    const auto document{parseOK(diags, "sensor { pixels 4 3 pitch 2 3 "
                                       "response { band v { 400 1 700 1 } } "
                                       "}\n")};
    CHECK(document.sensor.pitchUM.x == 2.0f);
    CHECK(document.sensor.pitchUM.y == 3.0f);
  }
  SUBCASE("A size in millimeters gives the pitch over the pixels") {
    const auto document{parseOK(diags, "sensor { pixels 6000 4000 size 36 24 "
                                       "response { band v { 400 1 700 1 } } "
                                       "}\n")};
    CHECK(document.sensor.pitchUM.x == doctest::Approx(6.0f));
    CHECK(document.sensor.pitchUM.y == doctest::Approx(6.0f));
  }
  SUBCASE("A size whose pitches disagree is refused, naming the pitch to "
          "write") {
    const auto error{parseError("sensor { pixels 6000 4000 size 36 20 "
                                "response { band v { 400 1 700 1 } } }\n")};
    CHECK_CONTAINS(error.message, "which is not square");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message, "state 'pitch 6 5'");
  }
  SUBCASE("Each thing the body needs is asked for by name") {
    CHECK_CONTAINS(parseError("sensor { pitch 2 response { band v { 400 1 "
                              "700 1 } } }\n")
                       .message,
                   "a sensor needs 'pixels'");
    CHECK_CONTAINS(parseError("sensor { pixels 4 3 response { band v { 400 "
                              "1 700 1 } } }\n")
                       .message,
                   "a sensor needs its pitch");
    CHECK_CONTAINS(parseError("sensor { pixels 4 3 pitch 2 }\n").message,
                   "needs a 'response' block");
    CHECK_CONTAINS(parseError(sensorWith("size 10 7.5")).message,
                   "'pitch' and 'size' both say how big a pixel is");
    CHECK_CONTAINS(parseError("sensor { pixels 4.5 3 pitch 2 response { "
                              "band v { 400 1 700 1 } } }\n")
                       .message,
                   "two positive integers for 'pixels'");
    CHECK_CONTAINS(parseError("sensor { pixels 4 3 pitch 0 response { band "
                              "v { 400 1 700 1 } } }\n")
                       .message,
                   "positive number for 'pitch'");
    CHECK_CONTAINS(parseError("sensor { pixels 4 3 pitch um response { band "
                              "v { 400 1 700 1 } } }\n")
                       .message,
                   "one or two positive numbers in micrometers after "
                   "'pitch'");
  }
  SUBCASE("The name, the readout, and its direction") {
    const auto document{parseOK(diags, sensorWith("name \"Body\" readout 0.03 "
                                                  "readout_direction left"))};
    CHECK(document.sensor.name == "Body");
    CHECK(document.sensor.readout == doctest::Approx(0.03f));
    CHECK(document.sensor.readoutDirection == ReadoutDirection::LEFT);
    CHECK_CONTAINS(parseError(sensorWith("readout -1")).message,
                   "nonnegative number for 'readout'");
    CHECK_CONTAINS(parseError(sensorWith("readout_direction sideways")).message,
                   "unknown readout direction 'sideways'");
  }
  SUBCASE("A key of the wrong block is sent to the right one") {
    CHECK_CONTAINS(parseError(sensorWith("kind qe")).message,
                   "belongs inside the 'response' block");
    CHECK_CONTAINS(parseError(sensorWith("full_well 3")).message,
                   "belongs inside the 'detector' block");
    CHECK_CONTAINS(parseError(sensorWith("focus 3")).message,
                   "unknown sensor setting 'focus'");
  }
}

TEST_CASE("SensorFile: the response block") {
  LayoutDiagnostics diags{};
  SUBCASE("It parses with its bands and their knots, and the kind is "
          "relative unless stated") {
    const auto document{parseOK(diags, "sensor {\n"
                                       "  pixels 4 3 pitch 2\n"
                                       "  response {\n"
                                       "    band vis { 380 0  550 1  720 0 }\n"
                                       "    band nir { 700 0 750 1 1000 1 }\n"
                                       "  }\n"
                                       "}\n")};
    const auto &response{document.sensor.response};
    CHECK(response.kind == ResponseKind::RELATIVE);
    CHECK(!response.peakQE);
    REQUIRE(response.bands.size() == 2);
    CHECK(response.bands[0].name == "vis");
    REQUIRE(response.bands[0].wavelengths.size() == 3);
    CHECK(response.bands[0].wavelengths[1] == 550.0f);
    CHECK(response.bands[0].values[1] == 1.0f);
    CHECK(response.bands[1].name == "nir");
    CHECK(response.bands[1].wavelengths[2] == 1000.0f);
    CHECK(!response.hasCFA());
    CHECK(response.bandIndex("nir") == 1);
    CHECK(!response.bandIndex("uv"));
    CHECK(!response.rgb);
    CHECK(!response.rgbBands());
  }
  SUBCASE("The kind is one of two words") {
    const auto document{parseOK(diags, responseWith("kind qe"))};
    CHECK(document.sensor.response.kind == ResponseKind::QE);
    CHECK_CONTAINS(parseError(responseWith("kind absolute")).message,
                   "unknown response kind 'absolute' (expected relative or "
                   "qe)");
  }
  SUBCASE("The peak quantum efficiency scales a relative curve, and only "
          "that") {
    const auto document{parseOK(diags, responseWith("peak_qe 0.8"))};
    REQUIRE(document.sensor.response.peakQE);
    CHECK(*document.sensor.response.peakQE == 0.8f);
    CHECK(document.sensor.response.qeScale() == doctest::Approx(0.8));
    CHECK_CONTAINS(parseError(responseWith("peak_qe 1.5")).message,
                   "expected 'peak_qe' between 0 and 1");
    CHECK_CONTAINS(parseError(responseWith("peak_qe 0")).message,
                   "expected 'peak_qe' between 0 and 1");
    CHECK_CONTAINS(parseError(responseWith("kind qe peak_qe 0.5")).message,
                   "'peak_qe' scales a 'relative' curve");
  }
  SUBCASE("A band needs pairs, at least two of them, ascending, finite, and "
          "nonnegative, each reported against the band") {
    for (const auto *text :
         {"sensor { pixels 4 3 pitch 2 response { band v { 400 1 700 } } }\n",
          "sensor { pixels 4 3 pitch 2 response { band v { 400 1 } } }\n",
          "sensor { pixels 4 3 pitch 2 response { band v { 700 1 400 1 } } "
          "}\n",
          "sensor { pixels 4 3 pitch 2 response { band v { 400 1 400 1 } } "
          "}\n",
          "sensor { pixels 4 3 pitch 2 response { band v { 400 -1 700 1 } } "
          "}\n",
          "sensor { pixels 4 3 pitch 2 response { band v { 400 inf 700 1 } "
          "} }\n",
          "sensor { pixels 4 3 pitch 2 response { band v { 0 1 700 1 } } "
          "}\n"}) {
      LayoutDiagnostics bad{};
      const auto &source{bad.addSource("test.sensor", text)};
      (void)parseSensor(bad, source);
      REQUIRE(bad.errorCount() == 1);
      const auto &error{bad.all().front()};
      CHECK_CONTAINS(error.message, "band 'v'");
      CHECK(error.location.offset == source.text.find("band"));
    }
    CHECK_CONTAINS(
        parseError("sensor { pixels 4 3 pitch 2 response { band v { 400 1 "
                   "700 } } }\n")
            .message,
        "wavelength and value pairs");
    CHECK_CONTAINS(parseError("sensor { pixels 4 3 pitch 2 response { band "
                              "v { 400 1 } } }\n")
                       .message,
                   "at least two pairs");
    CHECK_CONTAINS(parseError("sensor { pixels 4 3 pitch 2 response { band "
                              "v { 700 1 400 1 } } }\n")
                       .message,
                   "ascending wavelengths");
    CHECK_CONTAINS(parseError("sensor { pixels 4 3 pitch 2 response { band "
                              "v { 400 -1 700 1 } } }\n")
                       .message,
                   "finite nonnegative value at 400 nm");
  }
  SUBCASE("A stray word among the knots is an error where it sits") {
    CHECK_CONTAINS(parseError("sensor { pixels 4 3 pitch 2 response { band "
                              "v { 400 one 700 1 } } }\n")
                       .message,
                   "expected a number or '}' in band 'v', got 'one'");
  }
  SUBCASE("A band name is an identifier other than 'row', declared once") {
    CHECK_CONTAINS(parseError("sensor { pixels 4 3 pitch 2 response { band "
                              "7 { 400 1 700 1 } } }\n")
                       .message,
                   "expected a band name after 'band'");
    CHECK_CONTAINS(parseError("sensor { pixels 4 3 pitch 2 response { band "
                              "row { 400 1 700 1 } } }\n")
                       .message,
                   "expected a band name after 'band'");
    CHECK_CONTAINS(parseError(responseWith("band vis { 400 1 700 1 }")).message,
                   "band 'vis' is declared twice");
  }
  SUBCASE("A response needs a band, and does not name the body") {
    CHECK_CONTAINS(parseError("sensor { pixels 4 3 pitch 2 response { kind "
                              "qe } }\n")
                       .message,
                   "at least one 'band'");
    CHECK_CONTAINS(parseError(responseWith("name \"Body\"")).message,
                   "'name' is the sensor's, not the response's");
    CHECK_CONTAINS(parseError(responseWith("full_well 3")).message,
                   "unknown response setting 'full_well'");
  }
  SUBCASE("The tile parses row by row into band indices, whatever order "
          "the bands are declared in") {
    const auto document{parseOK(diags, "sensor {\n"
                                       "  pixels 4 3 pitch 2\n"
                                       "  response {\n"
                                       "    cfa { row R G  row G B }\n"
                                       "    band R { 400 1 700 1 }\n"
                                       "    band G { 400 1 700 1 }\n"
                                       "    band B { 400 1 700 1 }\n"
                                       "  }\n"
                                       "}\n")};
    const auto &response{document.sensor.response};
    REQUIRE(response.hasCFA());
    CHECK(response.cfaColumns == 2);
    CHECK(response.cfaRows() == 2);
    REQUIRE(response.cfa.size() == 4);
    CHECK(response.cfa[0] == 0);
    CHECK(response.cfa[1] == 1);
    CHECK(response.cfa[2] == 1);
    CHECK(response.cfa[3] == 2);
  }
  SUBCASE("The tile's rows agree in length, hold something, and name bands") {
    const auto tileWith{[](const char *cfa) {
      return std::string("sensor { pixels 4 3 pitch 2 response { band R { "
                         "400 1 700 1 } ") +
             cfa + " } }\n";
    }};
    CHECK_CONTAINS(parseError(tileWith("cfa { row R R  row R }")).message,
                   "expected 2 band names in this row, as in the first, "
                   "got 1");
    CHECK_CONTAINS(parseError(tileWith("cfa { row R row }")).message,
                   "at least one band name after 'row'");
    CHECK_CONTAINS(parseError(tileWith("cfa { }")).message,
                   "at least one 'row' in 'cfa'");
    CHECK_CONTAINS(parseError(tileWith("cfa { R R }")).message,
                   "expected 'row' in 'cfa', got 'R'");
    CHECK_CONTAINS(parseError(tileWith("cfa { row R Q }")).message,
                   "the tile names 'Q', which is not a band of this "
                   "response");
    CHECK_CONTAINS(parseError(tileWith("cfa { row R } cfa { row R }")).message,
                   "second 'cfa'");
  }
  SUBCASE("The three color bands are named, or follow the default") {
    const auto three{"sensor { pixels 4 3 pitch 2 response { band a { 400 1 "
                     "700 1 } band b { 400 1 700 1 } band c { 400 1 700 1 } "
                     "rgb c b a } }\n"};
    const auto named{parseOK(diags, three)};
    REQUIRE(named.sensor.response.rgb);
    CHECK((*named.sensor.response.rgb)[0] == 2);
    CHECK((*named.sensor.response.rgb)[2] == 0);
    CHECK(named.sensor.response.rgbBands() == named.sensor.response.rgb);
    // Unstated, the bands named R, G, and B, else the first three.
    const auto byName{parseOK(diags, "sensor { pixels 4 3 pitch 2 response "
                                     "{ band B { 400 1 700 1 } band G { "
                                     "400 1 700 1 } band R { 400 1 700 1 } "
                                     "} }\n")};
    REQUIRE(byName.sensor.response.rgbBands());
    CHECK((*byName.sensor.response.rgbBands())[0] == 2);
    CHECK((*byName.sensor.response.rgbBands())[2] == 0);
    const auto byOrder{parseOK(diags, "sensor { pixels 4 3 pitch 2 response "
                                      "{ band a { 400 1 700 1 } band b { "
                                      "400 1 700 1 } band c { 400 1 700 1 } "
                                      "band d { 400 1 700 1 } } }\n")};
    REQUIRE(byOrder.sensor.response.rgbBands());
    CHECK((*byOrder.sensor.response.rgbBands())[0] == 0);
    CHECK((*byOrder.sensor.response.rgbBands())[2] == 2);
    CHECK_CONTAINS(parseError(responseWith("rgb vis vis")).message,
                   "expected three band names after 'rgb'");
    CHECK_CONTAINS(parseError(responseWith("rgb vis vis Q")).message,
                   "'rgb' names 'Q', which is not a band");
    CHECK_CONTAINS(
        parseError(responseWith("rgb vis vis vis rgb vis vis vis")).message,
        "the second 'rgb'");
  }
  SUBCASE("A second response points at the first, and a path is refused") {
    const auto error{parseError(sensorWith("response { band w { 400 1 700 "
                                           "1 } }"))};
    CHECK_CONTAINS(error.message, "the second 'response'");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message, "the first one is here");
    const auto path{parseError("sensor { pixels 4 3 pitch 2 response "
                               "\"body.response\" }\n")};
    CHECK_CONTAINS(path.message, "'response' is a block here, not a path");
    REQUIRE(!path.notes.empty());
    CHECK_CONTAINS(path.notes.front().message, "a body is one file");
  }
}

TEST_CASE("SensorFile: the detector block") {
  LayoutDiagnostics diags{};
  SUBCASE("Absent, the detector is the generic one") {
    const auto document{parseOK(diags, BASE)};
    const auto &detector{document.sensor.detector};
    CHECK(!document.sensor.hasDetectorBlock);
    CHECK(!detector.baseISO);
    CHECK(!detector.fullWell);
    CHECK(detector.readNoise == 1.5f);
    CHECK(detector.darkCurrent == 0.1f);
    CHECK(detector.referenceTemperature == 25.0f);
    CHECK(detector.doublingTemperature == 7.0f);
    CHECK(detector.blackLevel == 128.0f);
    CHECK(detector.bits == 12);
    CHECK(detector.topCode() == 4095);
    CHECK(!detector.gain);
    CHECK(detector.maxISO == 102400.0f);
  }
  SUBCASE("An empty block is the generic detector too, and says it was "
          "written") {
    const auto document{parseOK(diags, detectorWith(""))};
    CHECK(document.sensor.hasDetectorBlock);
    CHECK(document.sensor.detector.blackLevel == 128.0f);
  }
  SUBCASE("Every key parses, last one wins") {
    const auto document{parseOK(
        diags, detectorWith("full_well 30000 read_noise 2.5 dark_current 0.02 "
                            "reference_temperature 5 doubling_temperature "
                            "12.7 black_level 64 bits 14 gain 0.5 bits 16 "
                            "max_iso 51200"))};
    const auto &detector{document.sensor.detector};
    REQUIRE(detector.fullWell);
    CHECK(*detector.fullWell == 30000.0f);
    CHECK(detector.readNoise == 2.5f);
    CHECK(detector.darkCurrent == 0.02f);
    CHECK(detector.referenceTemperature == 5.0f);
    CHECK(detector.doublingTemperature == 12.7f);
    CHECK(detector.blackLevel == 64.0f);
    CHECK(detector.bits == 16);
    REQUIRE(detector.gain);
    CHECK(*detector.gain == 0.5f);
    CHECK(detector.maxISO == 51200.0f);
  }
  SUBCASE("The black level follows the bits when unstated, whichever is "
          "written first") {
    CHECK(parseOK(diags, detectorWith("bits 14")).sensor.detector.blackLevel ==
          512.0f);
    CHECK(parseOK(diags, detectorWith("bits 8")).sensor.detector.blackLevel ==
          8.0f);
    CHECK(parseOK(diags, detectorWith("black_level 10 bits 14"))
              .sensor.detector.blackLevel == 10.0f);
    CHECK_CONTAINS(parseError(detectorWith("bits 8 black_level 255")).message,
                   "expected 'black_level' below the top code, which is 255 "
                   "at 8 bits");
  }
  SUBCASE("The base ISO and the well are one fact") {
    const auto document{parseOK(diags, detectorWith("base_iso 100"))};
    REQUIRE(document.sensor.detector.baseISO);
    CHECK(*document.sensor.detector.baseISO == 100.0f);
    const auto error{parseError(detectorWith("base_iso 100 full_well 50000"))};
    CHECK_CONTAINS(error.message, "'base_iso' and 'full_well' are one fact");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message, "'base_iso' is here");
    CHECK_CONTAINS(parseError(detectorWith("base_iso 800 max_iso 400")).message,
                   "expected 'max_iso' to be at least 'base_iso'");
  }
  SUBCASE("Each refusal points at its key") {
    CHECK_CONTAINS(parseError(detectorWith("full_well 0")).message,
                   "positive number for 'full_well'");
    CHECK_CONTAINS(parseError(detectorWith("full_well inf")).message,
                   "finite number for 'full_well'");
    CHECK_CONTAINS(parseError(detectorWith("read_noise -1")).message,
                   "nonnegative number for 'read_noise'");
    CHECK_CONTAINS(parseError(detectorWith("dark_current nan")).message,
                   "finite number for 'dark_current'");
    CHECK_CONTAINS(parseError(detectorWith("doubling_temperature 0")).message,
                   "positive number for 'doubling_temperature'");
    CHECK_CONTAINS(parseError(detectorWith("black_level -5")).message,
                   "nonnegative number for 'black_level'");
    CHECK_CONTAINS(parseError(detectorWith("bits 12.5")).message,
                   "integer from 1 to 16 for 'bits'");
    CHECK_CONTAINS(parseError(detectorWith("bits 17")).message,
                   "integer from 1 to 16 for 'bits'");
    CHECK_CONTAINS(parseError(detectorWith("gain 0")).message,
                   "positive number for 'gain'");
    CHECK_CONTAINS(parseError(detectorWith("gain")).message,
                   "expected 1 number, got 0 of them");
    CHECK_CONTAINS(parseError(detectorWith("well 3")).message,
                   "unknown detector setting 'well'");
    CHECK_CONTAINS(parseError(sensorWith("detector 3")).message,
                   "expected '{' after 'detector'");
  }
  SUBCASE("The temperature is the shot's, wherever it is written here") {
    const auto inDetector{parseError(detectorWith("temperature 30"))};
    CHECK_CONTAINS(inDetector.message, "not a fact about the detector");
    REQUIRE(!inDetector.notes.empty());
    CHECK_CONTAINS(inDetector.notes.front().message,
                   "state it in the 'camera' block");
    const auto inSensor{parseError(sensorWith("temperature 30"))};
    CHECK_CONTAINS(inSensor.message, "not a fact about the body");
  }
  SUBCASE("A second block points at the first") {
    const auto error{parseError(detectorWith("} detector { bits 8"))};
    CHECK_CONTAINS(error.message, "the second 'detector'");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message, "the first one is here");
  }
}

TEST_CASE("SensorFile: the file as a whole") {
  LayoutDiagnostics diags{};
  SUBCASE("A camera directive names the file it belongs in") {
    const auto error{parseError("camera { fovy 30 }\n")};
    CHECK_CONTAINS(error.message, "unknown directive 'camera'");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message,
                   "the '.camera' file that names this one");
  }
  SUBCASE("A response at the top level names the block it belongs in") {
    const auto error{parseError("response { band v { 400 1 700 1 } }\n")};
    CHECK_CONTAINS(error.message, "unknown directive 'response'");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message, "a block inside 'sensor'");
  }
  SUBCASE("A file with no block is an error, since a camera named it") {
    CHECK_CONTAINS(parseError("# nothing\n").message, "has none");
  }
  SUBCASE("A second block is an error rather than a merge") {
    const auto error{parseError(std::string(BASE) + BASE)};
    CHECK_CONTAINS(error.message, "the second 'sensor' block");
  }
  SUBCASE("It reads from disk, and resolves relative to the camera that "
          "names it") {
    TempDir tmpDir{"sensor-file"};
    const auto cameraPath{tmpDir.write("shot.camera",
                                       "camera { sensor \"bodies/body.sensor\" "
                                       "}\n")};
    const auto sensorPath{tmpDir.write("bodies/body.sensor", BASE)};
    const auto resolved{
        resolveSensorFileName("bodies/body.sensor", cameraPath.string())};
    CHECK(std::filesystem::path(resolved) == sensorPath);
    const auto document{readSensor(resolved)};
    CHECK(document.sensor.pixels.x == 4);
    CHECK(resolveSensorFileName("", cameraPath.string()) == "");
    CHECK_THROWS(
        (void)resolveSensorFileName("missing.sensor", cameraPath.string()));
    // An absolute path is taken as written.
    CHECK(resolveSensorFileName(sensorPath.string(), cameraPath.string()) ==
          sensorPath.string());
  }
  SUBCASE("The old response sidecar is named for what it became") {
    TempDir tmpDir{"sensor-file-response"};
    const auto path{tmpDir.write("body.response", "response { }\n")};
    const auto error{smdl::catchAndReturnError(
        [&] { (void)resolveSensorFileName(path.string(), ""); })};
    CHECK_ERROR(error,
                "names a '.response' file, a format that no longer exists");
    CHECK_ERROR(error, "the 'response' block of a '.sensor' file");
  }
  SUBCASE("A malformed file throws after reporting") {
    TempDir tmpDir{"sensor-file-bad"};
    const auto path{
        tmpDir.write("bad.sensor", "sensor { pixels 4 3 pitch 2 }\n")};
    CHECK_THROWS((void)readSensor(path.string()));
  }
  SUBCASE("A body as shipped parses end to end") {
    const auto document{parseOK(diags,
                                "# A body.\n"
                                "sensor {\n"
                                "  name \"Test body\"\n"
                                "  pixels 6000 4000\n"
                                "  pitch 5.93\n"
                                "  response {\n"
                                "    kind relative\n"
                                "    peak_qe 0.5\n"
                                "    band R { 380 0.001 550 0.3 780 0.01 }\n"
                                "    band G { 380 0.002 550 1.0 780 0.01 }\n"
                                "    band B { 380 0.5 550 0.2 780 0.001 }\n"
                                "    cfa { row R G  row G B }\n"
                                "  }\n"
                                "  detector {\n"
                                "    base_iso 100\n"
                                "    bits 14\n"
                                "  }\n"
                                "}\n")};
    const auto &sensor{document.sensor};
    CHECK(sensor.name == "Test body");
    CHECK(sensor.pixels.x == 6000);
    CHECK(sensor.sizeMM().x == doctest::Approx(35.58));
    CHECK(sensor.response.bands.size() == 3);
    CHECK(sensor.response.hasCFA());
    REQUIRE(sensor.response.rgbBands());
    CHECK((*sensor.response.rgbBands())[1] == 1);
    CHECK(sensor.response.qeScale() == doctest::Approx(0.5));
    CHECK(sensor.detector.bits == 14);
    CHECK(sensor.detector.blackLevel == 512.0f);
    REQUIRE(sensor.detector.baseISO);
  }
}
