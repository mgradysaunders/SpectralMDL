#include "Fixtures.h"

#include <cmath>
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

namespace {
// Parse from memory over a sink of its own, require exactly one error,
// and hand it back so the caller can say which one it expected. The
// sink is local so that a subcase can ask several times.
LayoutDiagnostic parseError(std::string text) {
  LayoutDiagnostics diags{};
  const auto &source{diags.addSource("test.camera", std::move(text))};
  (void)parseCamera(diags, source);
  REQUIRE(diags.errorCount() == 1);
  return diags.all().front();
}
} // namespace

TEST_CASE("CameraFile: the lens and the sensor") {
  LayoutDiagnostics diags{};
  SUBCASE("A lens is a path, kept as written for the reader to resolve, or "
          "the thin lens by name") {
    const auto document{
        parseOK(diags, "camera { lens \"lenses/dgauss-50mm.lens\" }\n")};
    REQUIRE(document.camera.lens);
    CHECK(*document.camera.lens == "lenses/dgauss-50mm.lens");
    const auto ideal{parseOK(diags, "camera { lens ideal }\n")};
    REQUIRE(ideal.camera.lens);
    CHECK(*ideal.camera.lens == LENS_IDEAL);
    CHECK_CONTAINS(parseError("camera { lens 50 }\n").message,
                   "expected a quoted '.lens' path or 'ideal' after 'lens'");
  }
  SUBCASE("A sensor is a path, or the observer by name") {
    const auto document{
        parseOK(diags, "camera { sensor \"bodies/a7m3.sensor\" }\n")};
    REQUIRE(document.camera.sensor);
    CHECK(*document.camera.sensor == "bodies/a7m3.sensor");
    const auto human{parseOK(diags, "camera { sensor human }\n")};
    REQUIRE(human.camera.sensor);
    CHECK(*human.camera.sensor == SENSOR_HUMAN);
    CHECK_CONTAINS(
        parseError("camera { sensor sony }\n").message,
        "expected a quoted '.sensor' path or 'human' after 'sensor'");
  }
  SUBCASE("Absent, both stay unset for the stand-ins to fill") {
    const auto document{parseOK(diags, "camera { fovy 30 }\n")};
    CHECK(!document.camera.lens);
    CHECK(!document.camera.sensor);
  }
  SUBCASE("A width and a height say where the frame went") {
    const auto error{parseError("camera { sensor 36 24 }\n")};
    CHECK_CONTAINS(error.message,
                   "'sensor' no longer takes a width and a height");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message,
                   "'.sensor' file's 'pixels' and 'pitch'");
  }
  SUBCASE("An inline block says that a body is a file") {
    const auto error{parseError("camera { sensor { pixels 4 3 } }\n")};
    CHECK_CONTAINS(error.message, "a body is a file");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message,
                   "write the block in a '.sensor' file");
  }
  SUBCASE("Neither can be keyed, being the instrument rather than a value "
          "in it") {
    for (const char *text : {"camera { motion { at 0 lens \"a.lens\" } }\n",
                             "camera { motion { at 0 sensor human } }\n"}) {
      CHECK_CONTAINS(parseError(text).message, "not a quantity to interpolate");
    }
  }
}

TEST_CASE("CameraFile: the keys that moved into the sensor file") {
  SUBCASE("A response, as a block or as a path, names where it went") {
    for (const char *text :
         {"camera { response { band v { 400 1 700 1 } } }\n",
          "camera { response \"body.response\" }\n",
          "camera { motion { at 0 response \"a.response\" } }\n"}) {
      const auto error{parseError(text)};
      CHECK_CONTAINS(error.message, "'response' is no longer a camera setting");
      REQUIRE(!error.notes.empty());
      CHECK_CONTAINS(error.notes.front().message,
                     "'response' block of the '.sensor' file");
    }
  }
  SUBCASE("A detector block likewise") {
    for (const char *text : {"camera { detector { bits 14 } }\n",
                             "camera { motion { at 0 detector { } } }\n"}) {
      const auto error{parseError(text)};
      CHECK_CONTAINS(error.message, "'detector' is no longer a camera setting");
      REQUIRE(!error.notes.empty());
      CHECK_CONTAINS(error.notes.front().message,
                     "'detector' block of the '.sensor' file");
    }
  }
  SUBCASE("At the top level, each names the block it belongs in") {
    const auto error{parseError("response { band v { 400 1 700 1 } }\n")};
    CHECK_CONTAINS(error.message, "unknown directive 'response'");
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message,
                   "block inside the 'sensor' block of the '.sensor' file");
    const auto stray{parseError("sensor \"a.sensor\"\n")};
    CHECK_CONTAINS(stray.message, "unknown directive 'sensor'");
    REQUIRE(!stray.notes.empty());
    CHECK_CONTAINS(stray.notes.front().message,
                   "a setting inside the 'camera' block");
  }
}

TEST_CASE("CameraFile: the temperature setting") {
  LayoutDiagnostics diags{};
  SUBCASE("It parses in degrees Celsius") {
    const auto document{parseOK(diags, "camera { temperature 35 }\n")};
    REQUIRE(document.camera.temperature);
    CHECK(*document.camera.temperature == 35.0f);
  }
  SUBCASE("Absent, it stays unset") {
    CHECK(!parseOK(diags, "camera { fovy 30 }\n").camera.temperature);
  }
  SUBCASE("A non-finite temperature is an error") {
    CHECK_CONTAINS(parseError("camera { temperature nan }\n").message,
                   "finite number for 'temperature'");
  }
  SUBCASE("It cannot be keyed, being the body's condition over the whole "
          "shot") {
    CHECK_CONTAINS(
        parseError("camera { motion { at 0 temperature 1 } }\n").message,
        "not a quantity to interpolate");
  }
}

TEST_CASE("CameraFile: the focal length setting") {
  LayoutDiagnostics diags{};
  SUBCASE("It parses in millimeters beside the field of view") {
    const auto document{parseOK(diags, "camera { focal_length 50 }\n")};
    REQUIRE(document.camera.focalLengthMM);
    CHECK(*document.camera.focalLengthMM == 50.0f);
    CHECK(!document.camera.fovYDeg);
  }
  SUBCASE("Absent, it stays unset") {
    CHECK(!parseOK(diags, "camera { fovy 30 }\n").camera.focalLengthMM);
  }
  SUBCASE("A zero is an error, like the field of view's") {
    CHECK_CONTAINS(parseError("camera { focal_length 0 }\n").message,
                   "positive number for 'focal_length'");
  }
  SUBCASE("It keys, being a quantity to interpolate: a zoom") {
    const auto document{parseOK(diags, "camera { motion { at 0 focal_length "
                                       "24 at 2 focal_length 70 } }\n")};
    CHECK(*document.camera.at(1.0f).focalLengthMM == doctest::Approx(47.0f));
    const auto held{document.camera.heldOverShutter(0.0f, 2.0f)};
    REQUIRE(held.size() == 1);
    CHECK(held.front() == "focal_length");
  }
}

TEST_CASE("CameraFile: the focus setting") {
  LayoutDiagnostics diags{};
  SUBCASE("A distance") {
    const auto document{parseOK(diags, "camera { focus 4 }\n")};
    REQUIRE(document.camera.focus);
    CHECK(*document.camera.focus == 4.0f);
    CHECK(!document.camera.shouldAutofocus);
  }
  SUBCASE("Infinity") {
    const auto document{parseOK(diags, "camera { focus infinity }\n")};
    REQUIRE(document.camera.focus);
    CHECK(std::isinf(*document.camera.focus));
    CHECK(!document.camera.shouldAutofocus);
  }
  SUBCASE("The autofocus, which leaves no distance") {
    const auto document{parseOK(diags, "camera { focus auto }\n")};
    CHECK(!document.camera.focus);
    CHECK(document.camera.shouldAutofocus);
  }
  SUBCASE("The last statement wins, either way round") {
    const auto toAuto{parseOK(diags, "camera { focus 4 focus auto }\n")};
    CHECK(!toAuto.camera.focus);
    CHECK(toAuto.camera.shouldAutofocus);
    const auto toDistance{parseOK(diags, "camera { focus auto }\n"
                                         "camera { focus 4 }\n")};
    CHECK(*toDistance.camera.focus == 4.0f);
    CHECK(!toDistance.camera.shouldAutofocus);
  }
  SUBCASE("Any other word names the three forms") {
    CHECK_CONTAINS(parseError("camera { focus near }\n").message,
                   "expected a distance, 'infinity', or 'auto' after 'focus'");
  }
  SUBCASE("A key states a distance, and neither word") {
    const auto document{
        parseOK(diags, "camera { motion { at 0 focus 2 at 1 focus 4 } }\n")};
    CHECK(*document.camera.at(0.5f).focus == doctest::Approx(3.0f));
    for (const char *word : {"auto", "infinity"}) {
      CAPTURE(word);
      const auto error{parseError(std::string("camera { motion { at 0 focus ") +
                                  word + " } }\n")};
      CHECK_CONTAINS(error.message, "cannot be keyed");
      REQUIRE(!error.notes.empty());
      CHECK_CONTAINS(error.notes.front().message, "'camera' block");
    }
  }
  SUBCASE("The autofocus beside a keyed focus is refused, whichever comes "
          "first") {
    for (const char *text :
         {"camera { focus auto motion { at 0 focus 2 } }\n",
          "camera { motion { at 0 focus 2 } focus auto }\n"}) {
      CAPTURE(text);
      const auto error{parseError(text)};
      CHECK_CONTAINS(error.message, "two statements of the focus");
      REQUIRE(!error.notes.empty());
    }
  }
}

TEST_CASE("CameraFile: the settings keep their locations") {
  LayoutDiagnostics diags{};
  const auto document{parseOK(diags, "camera {\n"
                                     "  fovy 30\n"
                                     "  focus 4\n"
                                     "  motion { at 0 fstop 2 }\n"
                                     "}\n"
                                     "camera { fovy 40 }\n")};
  SUBCASE("Each key's last statement, by line and column") {
    const auto fovy{document.keyLocs.find("fovy")};
    REQUIRE(fovy != document.keyLocs.end());
    const auto where{document.source->lineAndColumn(fovy->second.offset)};
    CHECK(where.lineNo == 6);
    CHECK(where.charNo == 10);
    CHECK(fovy->second.length == 4);
    const auto focus{document.keyLocs.find("focus")};
    REQUIRE(focus != document.keyLocs.end());
    CHECK(document.source->lineAndColumn(focus->second.offset).lineNo == 3);
  }
  SUBCASE("A setting stated only in a key has none") {
    CHECK(document.keyLocs.find("fstop") == document.keyLocs.end());
  }
  SUBCASE("The excerpt marks the key") {
    CHECK(LayoutDiagnostics::excerpt(document.keyLocs.at("focus")) ==
          "  focus 4\n  ^~~~~");
    CHECK(LayoutDiagnostics::where(document.keyLocs.at("focus")) ==
          "test.camera:3:3");
  }
}
