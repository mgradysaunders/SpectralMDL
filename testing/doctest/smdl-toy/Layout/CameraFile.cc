#include "Fixtures.h"

#include <filesystem>
#include <string>
#include <utility>

#include "Layout/CameraFile.h"

namespace {
// Parse from memory and require no errors.
CameraDocument parseOK(LayoutDiagnostics &diags, std::string text) {
  const auto &source{diags.addSource("test.camera", std::move(text))};
  auto document{parseCamera(diags, source)};
  if (diags.hasErrors()) MESSAGE(diags.renderAll(false));
  REQUIRE(!diags.hasErrors());
  return document;
}
} // namespace

TEST_CASE("CameraFile: the shutter setting") {
  LayoutDiagnostics diags{};
  SUBCASE("It parses, and the clock's other half is not the file's") {
    const auto document{parseOK(diags, "camera { shutter 0.02 }\n")};
    REQUIRE(document.camera.shutter);
    CHECK(*document.camera.shutter == doctest::Approx(0.02f));
  }
  SUBCASE("Absent, it stays unset") {
    const auto document{parseOK(diags, "camera { fovy 30 }\n")};
    CHECK(!document.camera.shutter);
  }
  SUBCASE("Two blocks merge per field, last one wins") {
    const auto document{parseOK(diags, "camera { shutter 0.5 fovy 30 }\n"
                                       "camera { shutter 0.25 }\n")};
    CHECK(*document.camera.shutter == doctest::Approx(0.25f));
    CHECK(*document.camera.fovYDeg == doctest::Approx(30.0f));
  }
  SUBCASE("A zero shutter is a shut shutter, not an error") {
    const auto document{parseOK(diags, "camera { shutter 0 }\n")};
    CHECK(*document.camera.shutter == 0.0f);
  }
  SUBCASE("A negative shutter is an error") {
    const auto &source{
        diags.addSource("test.camera", "camera { shutter -1 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message,
                   "nonnegative number for 'shutter'");
  }
  SUBCASE("A non-finite shutter is an error") {
    const auto &source{
        diags.addSource("test.camera", "camera { shutter inf }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "finite number for 'shutter'");
  }
  SUBCASE("It cannot be keyed, since it is the interval, not a value in it") {
    const auto &source{diags.addSource(
        "test.camera", "camera { motion { at 0 shutter 1 } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message,
                   "not a quantity to interpolate");
  }
}

TEST_CASE("CameraFile: the readout setting") {
  LayoutDiagnostics diags{};
  SUBCASE("It parses in seconds beside the shutter, and the direction "
          "stays unset") {
    const auto document{
        parseOK(diags, "camera { shutter 0.01 readout 0.03 }\n")};
    REQUIRE(document.camera.readout);
    CHECK(*document.camera.readout == doctest::Approx(0.03f));
    CHECK(!document.camera.readoutDirection);
  }
  SUBCASE("Absent, it stays unset") {
    const auto document{parseOK(diags, "camera { shutter 0.01 }\n")};
    CHECK(!document.camera.readout);
    CHECK(!document.camera.readoutDirection);
  }
  SUBCASE("A zero readout is a global shutter, not an error") {
    const auto document{parseOK(diags, "camera { readout 0 }\n")};
    CHECK(*document.camera.readout == 0.0f);
  }
  SUBCASE("A negative readout is an error") {
    const auto &source{
        diags.addSource("test.camera", "camera { readout -1 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message,
                   "nonnegative number for 'readout'");
  }
  SUBCASE("A non-finite readout is an error") {
    const auto &source{
        diags.addSource("test.camera", "camera { readout nan }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "finite number for 'readout'");
  }
  SUBCASE("The direction is one of four words") {
    for (const auto &[word, direction] :
         {std::pair{"down", ReadoutDirection::DOWN},
          std::pair{"up", ReadoutDirection::UP},
          std::pair{"left", ReadoutDirection::LEFT},
          std::pair{"right", ReadoutDirection::RIGHT}}) {
      const auto document{parseOK(
          diags, std::string("camera { readout_direction ") + word + " }\n")};
      REQUIRE(document.camera.readoutDirection);
      CHECK(*document.camera.readoutDirection == direction);
    }
  }
  SUBCASE("Any other direction word is an error naming the four") {
    const auto &source{diags.addSource(
        "test.camera", "camera { readout_direction sideways }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message,
                   "unknown readout direction 'sideways'");
    CHECK_CONTAINS(diags.all().front().message, "down, up, left, or right");
  }
  SUBCASE("Neither can be keyed, since they describe the interval, not a "
          "value in it") {
    for (const auto *text :
         {"camera { motion { at 0 readout 1 } }\n",
          "camera { motion { at 0 readout_direction up } }\n"}) {
      LayoutDiagnostics keyed{};
      const auto &source{keyed.addSource("test.camera", text)};
      (void)parseCamera(keyed, source);
      REQUIRE(keyed.errorCount() == 1);
      CHECK_CONTAINS(keyed.all().front().message,
                     "not a quantity to interpolate");
    }
  }
}

TEST_CASE("CameraFile: what the file deliberately does not carry") {
  LayoutDiagnostics diags{};
  SUBCASE("A 'time' directive names where the clock comes from instead") {
    const auto &source{
        diags.addSource("test.camera", "time { base 2 shutter 0.02 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK_CONTAINS(error.message, "unknown directive");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message, "'-time' names the instant");
  }
  SUBCASE("'resolution' names the flag that sizes the picture") {
    const auto &source{
        diags.addSource("test.camera", "camera { resolution 640 480 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK_CONTAINS(error.message, "fact about this render");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message, "-resolution");
    CHECK_CONTAINS(error.notes.front().message, "-crop-window");
  }
  SUBCASE("The parse resynchronizes at the next statement") {
    const auto &source{diags.addSource(
        "test.camera", "time { base 2 }\ncamera { fovy 30 }\n")};
    const auto document{parseCamera(diags, source)};
    REQUIRE(diags.errorCount() == 1);
    REQUIRE(document.camera.fovYDeg);
    CHECK(*document.camera.fovYDeg == doctest::Approx(30.0f));
  }
}

TEST_CASE("CameraFile: the camera motion block") {
  LayoutDiagnostics diags{};
  SUBCASE("Absent, there is no motion") {
    const auto document{parseOK(diags, "camera { look_from 1 2 3 }\n")};
    CHECK(document.camera.motion.empty());
  }
  SUBCASE("Two keys, and each holds only what it states") {
    const auto document{parseOK(diags, "camera {\n"
                                       "  look_from -6 0 2\n"
                                       "  motion {\n"
                                       "    at 1 look_to 0 0 0\n"
                                       "    at 2 look_to 0 4 0\n"
                                       "  }\n"
                                       "}\n")};
    const auto &camera{document.camera};
    REQUIRE(camera.motion.size() == 2);
    CHECK(camera.motion[0].time == doctest::Approx(1.0f));
    CHECK(camera.motion[1].time == doctest::Approx(2.0f));
    CHECK(!camera.motion[0].lookFrom);
    REQUIRE(camera.motion[1].lookTo);
    CHECK(camera.motion[1].lookTo->y == doctest::Approx(4.0f));
    // A setting no key states holds the block's own value at every time.
    CHECK(camera.at(1.0f).lookFrom->x == doctest::Approx(-6.0f));
    CHECK(camera.at(2.0f).lookFrom->x == doctest::Approx(-6.0f));
    // A keyed setting interpolates between the two that surround the
    // time, and clamps outside them.
    CHECK(camera.at(1.5f).lookTo->y == doctest::Approx(2.0f));
    CHECK(camera.at(0.0f).lookTo->y == doctest::Approx(0.0f));
    CHECK(camera.at(9.0f).lookTo->y == doctest::Approx(4.0f));
  }
  SUBCASE("A key states three settings at once") {
    const auto document{parseOK(diags, "camera { motion { at 0 look_from 1 2 3 "
                                       "look_to 4 5 6 look_up 7 8 9 } }\n")};
    const auto &key{document.camera.motion.at(0)};
    REQUIRE(key.lookFrom);
    REQUIRE(key.lookTo);
    REQUIRE(key.lookUp);
    CHECK(key.lookFrom->z == doctest::Approx(3.0f));
    CHECK(key.lookTo->z == doctest::Approx(6.0f));
    CHECK(key.lookUp->z == doctest::Approx(9.0f));
  }
  SUBCASE("The lens keys too, and what cannot vary is reported") {
    const auto document{parseOK(diags, "camera {\n"
                                       "  motion {\n"
                                       "    at 0 focus 4 look_from 0 0 0\n"
                                       "    at 4 focus 8 look_from 1 0 0\n"
                                       "  }\n"
                                       "}\n")};
    const auto &camera{document.camera};
    CHECK(camera.at(2.0f).focus == doctest::Approx(6.0f));
    const auto held{camera.heldOverShutter(0.0f, 4.0f)};
    REQUIRE(held.size() == 1);
    CHECK(held.front() == "focus");
    // The framing is what the shutter interpolates, so it is never held.
    CHECK(camera.heldOverShutter(0.0f, 0.0f).empty());
    CHECK(camera.hasKeyBetween(-1.0f, 1.0f));
    CHECK(!camera.hasKeyBetween(0.0f, 4.0f));
  }
  SUBCASE("A key at its own time is that key exactly") {
    const auto document{parseOK(diags, "camera { motion { at 0.5 fovy 30 "
                                       "at 1.5 fovy 60 } }\n")};
    CHECK(*document.camera.at(0.5f).fovYDeg == 30.0f);
    CHECK(*document.camera.at(1.5f).fovYDeg == 60.0f);
  }
  SUBCASE("Keys are written in ascending time") {
    const auto &source{diags.addSource(
        "test.camera", "camera { motion { at 2 fovy 30 at 1 fovy 60 } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "ascending time");
  }
  SUBCASE("A setting before the first key names the spelling") {
    const auto &source{diags.addSource(
        "test.camera", "camera { motion { look_to 0 0 0 } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK_CONTAINS(error.message, "'at <seconds>' keys");
    REQUIRE(!error.notes.empty());
  }
  SUBCASE("An empty block is an error rather than a still camera") {
    const auto &source{
        diags.addSource("test.camera", "camera { motion { } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "at least one");
  }
  SUBCASE("What cannot be interpolated cannot be keyed") {
    const auto &source{diags.addSource(
        "test.camera", "camera { motion { at 0 resolution 64 64 } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message,
                   "not a quantity to interpolate");
  }
  SUBCASE("An unknown setting inside a key is an error") {
    const auto &source{
        diags.addSource("test.camera", "camera { motion { at 0 fov 30 } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "in a 'motion' key");
  }
  SUBCASE("The block needs its brace") {
    const auto &source{diags.addSource(
        "test.camera", "camera { motion at 0 look_to 0 0 0 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "'{' after 'motion'");
  }
  SUBCASE("At the top level, motion is still an unknown directive") {
    const auto &source{
        diags.addSource("test.camera", "motion { at 0 look_to 0 0 0 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "unknown directive");
  }
}

TEST_CASE("CameraFile: the camera block") {
  LayoutDiagnostics diags{};
  SUBCASE("The framing and the lens parse, and the location is the first") {
    const auto document{parseOK(diags, "camera {\n"
                                       "  look_from -6 0 2\n"
                                       "  look_to 0 0 0.5\n"
                                       "  look_up 0 0 1\n"
                                       "  fovy 45  focus 4  aperture 0.01\n"
                                       "  distortion_fit\n"
                                       "}\n")};
    const auto &camera{document.camera};
    REQUIRE(camera.lookFrom);
    CHECK(camera.lookFrom->x == doctest::Approx(-6.0f));
    REQUIRE(camera.lookTo);
    CHECK(camera.lookTo->z == doctest::Approx(0.5f));
    REQUIRE(camera.lookUp);
    CHECK(camera.lookUp->z == doctest::Approx(1.0f));
    CHECK(*camera.fovYDeg == doctest::Approx(45.0f));
    CHECK(*camera.focus == doctest::Approx(4.0f));
    CHECK(*camera.aperture == doctest::Approx(0.01f));
    CHECK(*camera.shouldFitDistortion == true);
    CHECK(!camera.fStop);
    REQUIRE(document.cameraLoc);
    CHECK(document.source->lineAndColumn(document.cameraLoc.offset).lineNo ==
          1);
  }
  SUBCASE("Everything is unset by default, for the merge to fill") {
    const auto document{parseOK(diags, "")};
    CHECK(!document.camera.lookFrom);
    CHECK(!document.camera.shutter);
    CHECK(!document.cameraLoc);
  }
  SUBCASE("A zero for a quantity that derives a default is an error") {
    const auto &source{diags.addSource("test.camera", "camera { fovy 0 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "positive number for");
  }
  SUBCASE("A scene directive names where it belongs") {
    const auto &source{
        diags.addSource("test.camera", "place rock translate 0 0 1\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK_CONTAINS(error.message, "unknown directive");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message, "belongs in the layout");
  }
}

TEST_CASE("CameraFile: the lens and the sensor") {
  LayoutDiagnostics diags{};
  SUBCASE("A lens is a path, kept as written for the reader to resolve") {
    const auto document{
        parseOK(diags, "camera { lens \"lenses/dgauss-50mm.lens\" }\n")};
    REQUIRE(document.camera.lens);
    CHECK(*document.camera.lens == "lenses/dgauss-50mm.lens");
  }
  SUBCASE("A sensor is a width and a height in millimeters") {
    const auto document{parseOK(diags, "camera { sensor 36 24 }\n")};
    REQUIRE(document.camera.sensorMM);
    CHECK(document.camera.sensorMM->x == doctest::Approx(36.0f));
    CHECK(document.camera.sensorMM->y == doctest::Approx(24.0f));
  }
  SUBCASE("A sensor with no size is an error") {
    const auto &source{
        diags.addSource("test.camera", "camera { sensor 36 0 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "positive number for 'sensor'");
  }
  SUBCASE("Neither can be keyed, being the instrument rather than a value "
          "in it") {
    for (const char *text : {"camera { motion { at 0 lens \"a.lens\" } }\n",
                             "camera { motion { at 0 sensor 36 24 } }\n"}) {
      LayoutDiagnostics keyed{};
      const auto &source{keyed.addSource("test.camera", text)};
      (void)parseCamera(keyed, source);
      REQUIRE(keyed.errorCount() == 1);
      CHECK_CONTAINS(keyed.all().front().message,
                     "not a quantity to interpolate");
    }
  }
}

namespace {
// The shortest thing that is a response: one band of two knots. The base
// every malformed case below perturbs.
constexpr const char *ONE_BAND = "camera {\n"
                                 "  response { band vis { 400 1 700 1 } }\n"
                                 "}\n";

// Parse from memory over a sink of its own, require exactly one error,
// and hand back its message so the caller can say which one it
// expected. The sink is local so that a subcase can ask several times.
std::string parseError(std::string text) {
  LayoutDiagnostics diags{};
  const auto &source{diags.addSource("test.camera", std::move(text))};
  (void)parseCamera(diags, source);
  REQUIRE(diags.errorCount() == 1);
  return diags.all().front().message;
}
} // namespace

TEST_CASE("CameraFile: the response block") {
  LayoutDiagnostics diags{};
  SUBCASE("It parses with its name, its bands, and their knots, and the "
          "kind is relative unless stated") {
    const auto document{parseOK(diags, "camera {\n"
                                       "  response {\n"
                                       "    name \"Two bands\"\n"
                                       "    band vis { 380 0  550 1  720 0 }\n"
                                       "    band nir { 700 0 750 1 1000 1 }\n"
                                       "  }\n"
                                       "}\n")};
    REQUIRE(document.camera.response);
    const auto &response{*document.camera.response};
    CHECK(response.name == "Two bands");
    CHECK(response.kind == ResponseKind::RELATIVE);
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
    CHECK(!document.camera.responseFile);
  }
  SUBCASE("Absent, there is none") {
    const auto document{parseOK(diags, "camera { fovy 30 }\n")};
    CHECK(!document.camera.response);
    CHECK(!document.camera.responseFile);
  }
  SUBCASE("The kind is one of two words") {
    const auto document{parseOK(diags, "camera { response { kind qe band e "
                                       "{ 400 0.5 700 0.5 } } }\n")};
    CHECK(document.camera.response->kind == ResponseKind::QE);
    CHECK_CONTAINS(parseError("camera { response { kind absolute band "
                              "e { 400 0.5 700 0.5 } } }\n"),
                   "unknown response kind 'absolute' (expected relative or "
                   "qe)");
  }
  SUBCASE("A band needs pairs, at least two of them, ascending, finite, and "
          "nonnegative, each reported against the band") {
    for (const auto *text :
         {"camera { response { band v { 400 1 700 } } }\n",
          "camera { response { band v { 400 1 } } }\n",
          "camera { response { band v { 700 1 400 1 } } }\n",
          "camera { response { band v { 400 1 400 1 } } }\n",
          "camera { response { band v { 400 -1 700 1 } } }\n",
          "camera { response { band v { 400 inf 700 1 } } }\n",
          "camera { response { band v { 0 1 700 1 } } }\n"}) {
      LayoutDiagnostics bad{};
      const auto &source{bad.addSource("test.camera", text)};
      (void)parseCamera(bad, source);
      REQUIRE(bad.errorCount() == 1);
      const auto &error{bad.all().front()};
      CHECK_CONTAINS(error.message, "band 'v'");
      CHECK(error.location.offset == source.text.find("band"));
    }
    CHECK_CONTAINS(parseError("camera { response { band v { 400 1 700 "
                              "} } }\n"),
                   "wavelength and value pairs");
    CHECK_CONTAINS(parseError("camera { response { band v { 400 1 } } }\n"),
                   "at least two pairs");
    CHECK_CONTAINS(
        parseError("camera { response { band v { 700 1 400 1 } } }\n"),
        "ascending wavelengths");
    CHECK_CONTAINS(
        parseError("camera { response { band v { 400 -1 700 1 } } }\n"),
        "finite nonnegative value at 400 nm");
  }
  SUBCASE("A stray word among the knots is an error where it sits") {
    CHECK_CONTAINS(parseError("camera { response { band v { 400 one "
                              "700 1 } } }\n"),
                   "expected a number or '}' in band 'v', got 'one'");
  }
  SUBCASE("A band name is an identifier other than 'row', declared once") {
    CHECK_CONTAINS(parseError("camera { response { band 7 { 400 1 700 "
                              "1 } } }\n"),
                   "expected a band name after 'band'");
    CHECK_CONTAINS(parseError("camera { response { band row { 400 1 "
                              "700 1 } } }\n"),
                   "expected a band name after 'band'");
    CHECK_CONTAINS(parseError("camera { response { band v { 400 1 700 "
                              "1 } band v { 400 1 700 1 } } }\n"),
                   "band 'v' is declared twice");
  }
  SUBCASE("A response needs a band") {
    CHECK_CONTAINS(parseError("camera { response { kind qe } }\n"),
                   "at least one 'band'");
  }
  SUBCASE("The tile parses row by row into band indices, whatever order "
          "the bands are declared in") {
    const auto document{parseOK(diags, "camera {\n"
                                       "  response {\n"
                                       "    cfa { row R G  row G B }\n"
                                       "    band R { 400 1 700 1 }\n"
                                       "    band G { 400 1 700 1 }\n"
                                       "    band B { 400 1 700 1 }\n"
                                       "  }\n"
                                       "}\n")};
    const auto &response{*document.camera.response};
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
    CHECK_CONTAINS(parseError("camera { response { band R { 400 1 700 "
                              "1 } cfa { row R R  row R } } }\n"),
                   "expected 2 band name(s) in this row, as in the first, "
                   "got 1");
    CHECK_CONTAINS(parseError("camera { response { band R { 400 1 700 "
                              "1 } cfa { row R row } } }\n"),
                   "at least one band name after 'row'");
    CHECK_CONTAINS(parseError("camera { response { band R { 400 1 700 "
                              "1 } cfa { } } }\n"),
                   "at least one 'row' in 'cfa'");
    CHECK_CONTAINS(parseError("camera { response { band R { 400 1 700 "
                              "1 } cfa { R R } } }\n"),
                   "expected 'row' in 'cfa', got 'R'");
    CHECK_CONTAINS(parseError("camera { response { band R { 400 1 700 "
                              "1 } cfa { row R Q } } }\n"),
                   "the tile names 'Q', which is not a band of this "
                   "response");
    CHECK_CONTAINS(parseError("camera { response { band R { 400 1 700 "
                              "1 } cfa { row R } cfa { row R } } }\n"),
                   "second 'cfa'");
  }
  SUBCASE("A quoted path names a sidecar instead") {
    const auto document{
        parseOK(diags, "camera { response \"sensors/body.response\" }\n")};
    CHECK(!document.camera.response);
    REQUIRE(document.camera.responseFile);
    CHECK(*document.camera.responseFile == "sensors/body.response");
  }
  SUBCASE("Anything else after 'response' is an error") {
    CHECK_CONTAINS(parseError("camera { response relative }\n"),
                   "expected '{' or a quoted '.response' path after "
                   "'response'");
  }
  SUBCASE("A second response of either form points at the first") {
    for (const auto *text :
         {"camera { response \"a.response\" response \"b.response\" }\n",
          "camera { response \"a.response\" response { band v { 400 1 700 1 "
          "} } }\n",
          "camera { response { band v { 400 1 700 1 } } }\n"
          "camera { response { band w { 400 1 700 1 } } }\n"}) {
      LayoutDiagnostics twice{};
      const auto &source{twice.addSource("test.camera", text)};
      (void)parseCamera(twice, source);
      REQUIRE(twice.errorCount() == 1);
      const auto &error{twice.all().front()};
      CHECK_CONTAINS(error.message, "second 'response'");
      REQUIRE(!error.notes.empty());
      CHECK_CONTAINS(error.notes.front().message, "the first one is here");
    }
  }
  SUBCASE("It cannot be keyed, being the instrument rather than a value "
          "in it") {
    CHECK_CONTAINS(parseError("camera { motion { at 0 response "
                              "\"a.response\" } }\n"),
                   "not a quantity to interpolate");
  }
  SUBCASE("At the top level of a camera file it names where it belongs") {
    LayoutDiagnostics top{};
    const auto &source{top.addSource("test.camera", "response { band v { 400 "
                                                    "1 700 1 } }\n")};
    (void)parseCamera(top, source);
    REQUIRE(top.errorCount() == 1);
    const auto &error{top.all().front()};
    CHECK_CONTAINS(error.message, "unknown directive 'response'");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message,
                   "inside the 'camera' block, or in a '.response' file");
  }
  SUBCASE("The base parses, which is what makes the perturbations above "
          "meaningful") {
    const auto document{parseOK(diags, ONE_BAND)};
    REQUIRE(document.camera.response);
    CHECK(document.camera.response->bands.size() == 1);
  }
}

TEST_CASE("CameraFile: the response sidecar") {
  LayoutDiagnostics diags{};
  SUBCASE("A response file is the block at the top level") {
    const auto &source{diags.addSource("body.response",
                                       "response {\n"
                                       "  name \"Body\"\n"
                                       "  band R { 400 0.1 700 0.9 }\n"
                                       "  band G { 400 0.5 700 0.5 }\n"
                                       "}\n")};
    const auto document{parseResponse(diags, source)};
    if (diags.hasErrors()) MESSAGE(diags.renderAll(false));
    REQUIRE(!diags.hasErrors());
    CHECK(document.response.name == "Body");
    REQUIRE(document.response.bands.size() == 2);
    CHECK(document.response.bands[1].name == "G");
    REQUIRE(document.responseLoc);
    CHECK(document.source->lineAndColumn(document.responseLoc.offset).lineNo ==
          1);
  }
  SUBCASE("A camera directive there names the file it belongs in") {
    const auto &source{
        diags.addSource("body.response", "camera { fovy 30 }\n")};
    (void)parseResponse(diags, source);
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK_CONTAINS(error.message, "unknown directive 'camera'");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message,
                   "the '.camera' file that names this one");
  }
  SUBCASE("A file with no block is an error, since a camera named it") {
    const auto &source{diags.addSource("body.response", "# nothing\n")};
    (void)parseResponse(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "has none");
  }
  SUBCASE("A second block is an error rather than a merge") {
    const auto &source{diags.addSource(
        "body.response", "response { band R { 400 1 700 1 } }\n"
                         "response { band G { 400 1 700 1 } }\n")};
    (void)parseResponse(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "second 'response' block");
  }
  SUBCASE("It reads from disk, and resolves relative to the camera that "
          "names it") {
    TempDir tmpDir{"camera-response"};
    const auto cameraPath{tmpDir.write("shot.camera", "camera { response "
                                                      "\"sensors/body.response"
                                                      "\" }\n")};
    const auto responsePath{tmpDir.write(
        "sensors/body.response", "response { band R { 400 1 700 1 } }\n")};
    const auto resolved{resolveResponseFileName("", cameraPath.string(),
                                                "sensors/body.response")};
    CHECK(std::filesystem::path(resolved) == responsePath);
    const auto document{readResponse(resolved)};
    REQUIRE(document.response.bands.size() == 1);
    CHECK(document.response.bands[0].name == "R");
    CHECK(resolveResponseFileName("", cameraPath.string(), "") == "");
    CHECK_THROWS((void)resolveResponseFileName("", cameraPath.string(),
                                               "missing.response"));
    CHECK_THROWS((void)resolveResponseFileName("missing.response",
                                               cameraPath.string(), ""));
    // The flag wins over whatever the file states, as written.
    CHECK(resolveResponseFileName(responsePath.string(), cameraPath.string(),
                                  "other.response") == responsePath.string());
  }
  SUBCASE("A malformed file throws after reporting") {
    TempDir tmpDir{"camera-response-bad"};
    const auto path{
        tmpDir.write("bad.response", "response { band R { 400 1 } }\n")};
    CHECK_THROWS((void)readResponse(path.string()));
  }
}
