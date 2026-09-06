#include "doctest.h"

#include <string>

#include "Layout/CameraFile.h"

// Parse from memory and require no errors.
static CameraDocument parseOK(LayoutDiagnostics &diags, std::string text) {
  const auto &source{diags.addSource("test.camera", std::move(text))};
  auto document{parseCamera(diags, source)};
  if (diags.hasErrors()) MESSAGE(diags.renderAll(false));
  REQUIRE(!diags.hasErrors());
  return document;
}

TEST_CASE("CameraFile: the shutter setting") {
  LayoutDiagnostics diags{};
  SUBCASE("It parses, and the clock's other half is not the file's") {
    const auto document{
        parseOK(diags, "#smdl camera\ncamera { shutter 0.02 }\n")};
    REQUIRE(document.camera.shutter);
    CHECK(*document.camera.shutter == doctest::Approx(0.02f));
  }
  SUBCASE("Absent, it stays unset") {
    const auto document{parseOK(diags, "#smdl camera\ncamera { fovy 30 }\n")};
    CHECK(!document.camera.shutter);
  }
  SUBCASE("Two blocks merge per field, last one wins") {
    const auto document{parseOK(diags, "#smdl camera\n"
                                       "camera { shutter 0.5 fovy 30 }\n"
                                       "camera { shutter 0.25 }\n")};
    CHECK(*document.camera.shutter == doctest::Approx(0.25f));
    CHECK(*document.camera.fovYDeg == doctest::Approx(30.0f));
  }
  SUBCASE("A zero shutter is a shut shutter, not an error") {
    const auto document{parseOK(diags, "#smdl camera\ncamera { shutter 0 }\n")};
    CHECK(*document.camera.shutter == 0.0f);
  }
  SUBCASE("A negative shutter is an error") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\ncamera { shutter -1 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find(
              "nonnegative number for 'shutter'") != std::string::npos);
  }
  SUBCASE("A non-finite shutter is an error") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\ncamera { shutter inf }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("finite number for 'shutter'") !=
          std::string::npos);
  }
  SUBCASE("It cannot be keyed, since it is the interval, not a value in it") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\ncamera { motion { at 0 shutter 1 } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("not a quantity to interpolate") !=
          std::string::npos);
  }
}

TEST_CASE("CameraFile: what the file deliberately does not carry") {
  LayoutDiagnostics diags{};
  SUBCASE("A 'time' directive names where the clock comes from instead") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\ntime { base 2 shutter 0.02 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK(error.message.find("unknown directive") != std::string::npos);
    REQUIRE(!error.notes.empty());
    CHECK(error.notes.front().message.find("'-time' names the instant") !=
          std::string::npos);
  }
  SUBCASE("'resolution' names the flag that sizes the picture") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\ncamera { resolution 640 480 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK(error.message.find("fact about this render") != std::string::npos);
    REQUIRE(!error.notes.empty());
    CHECK(error.notes.front().message.find("-resolution") != std::string::npos);
    CHECK(error.notes.front().message.find("-crop-window") !=
          std::string::npos);
  }
  SUBCASE("The parse resynchronizes at the next statement") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\ntime { base 2 }\ncamera { fovy 30 }\n")};
    const auto document{parseCamera(diags, source)};
    REQUIRE(diags.errorCount() == 1);
    REQUIRE(document.camera.fovYDeg);
    CHECK(*document.camera.fovYDeg == doctest::Approx(30.0f));
  }
}

TEST_CASE("CameraFile: the camera motion block") {
  LayoutDiagnostics diags{};
  SUBCASE("Absent, there is no motion") {
    const auto document{
        parseOK(diags, "#smdl camera\ncamera { look_from 1 2 3 }\n")};
    CHECK(document.camera.motion.empty());
  }
  SUBCASE("Two keys, and each holds only what it states") {
    const auto document{parseOK(diags, "#smdl camera\ncamera {\n"
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
    const auto document{
        parseOK(diags, "#smdl camera\ncamera { motion { at 0 look_from 1 2 3 "
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
    const auto document{parseOK(diags, "#smdl camera\ncamera {\n"
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
    const auto document{
        parseOK(diags, "#smdl camera\ncamera { motion { at 0.5 fovy 30 "
                       "at 1.5 fovy 60 } }\n")};
    CHECK(*document.camera.at(0.5f).fovYDeg == 30.0f);
    CHECK(*document.camera.at(1.5f).fovYDeg == 60.0f);
  }
  SUBCASE("Keys are written in ascending time") {
    const auto &source{diags.addSource(
        "test.camera",
        "#smdl camera\ncamera { motion { at 2 fovy 30 at 1 fovy 60 } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("ascending time") !=
          std::string::npos);
  }
  SUBCASE("A setting before the first key names the spelling") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\ncamera { motion { look_to 0 0 0 } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK(error.message.find("'at <seconds>' keys") != std::string::npos);
    REQUIRE(!error.notes.empty());
  }
  SUBCASE("An empty block is an error rather than a still camera") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\ncamera { motion { } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("at least one") !=
          std::string::npos);
  }
  SUBCASE("What cannot be interpolated cannot be keyed") {
    const auto &source{diags.addSource(
        "test.camera",
        "#smdl camera\ncamera { motion { at 0 resolution 64 64 } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("not a quantity to interpolate") !=
          std::string::npos);
  }
  SUBCASE("An unknown setting inside a key is an error") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\ncamera { motion { at 0 fov 30 } }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("in a 'motion' key") !=
          std::string::npos);
  }
  SUBCASE("The block needs its brace") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\ncamera { motion at 0 look_to 0 0 0 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("'{' after 'motion'") !=
          std::string::npos);
  }
  SUBCASE("At the top level, motion is still an unknown directive") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\nmotion { at 0 look_to 0 0 0 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("unknown directive") !=
          std::string::npos);
  }
}

TEST_CASE("CameraFile: the camera block") {
  LayoutDiagnostics diags{};
  SUBCASE("The framing and the lens parse, and the location is the first") {
    const auto document{parseOK(diags, "#smdl camera\ncamera {\n"
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
    CHECK(*camera.distortionFit == true);
    CHECK(!camera.fStop);
    REQUIRE(document.cameraLoc);
    CHECK(document.source->lineAndColumn(document.cameraLoc.offset).lineNo ==
          2);
  }
  SUBCASE("Everything is unset by default, for the merge to fill") {
    const auto document{parseOK(diags, "#smdl camera\n")};
    CHECK(!document.camera.lookFrom);
    CHECK(!document.camera.shutter);
    CHECK(!document.cameraLoc);
  }
  SUBCASE("A zero for a quantity that derives a default is an error") {
    const auto &source{
        diags.addSource("test.camera", "#smdl camera\ncamera { fovy 0 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("positive number for") !=
          std::string::npos);
  }
  SUBCASE("The magic is the camera's own") {
    const auto &source{
        diags.addSource("test.camera", "#smdl layout\ncamera { fovy 30 }\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() >= 1);
    CHECK(diags.all().front().message.find("#smdl camera") !=
          std::string::npos);
  }
  SUBCASE("A scene directive names where it belongs") {
    const auto &source{diags.addSource(
        "test.camera", "#smdl camera\nplace rock translate 0 0 1\n")};
    (void)parseCamera(diags, source);
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK(error.message.find("unknown directive") != std::string::npos);
    REQUIRE(!error.notes.empty());
    CHECK(error.notes.front().message.find("belongs in the layout") !=
          std::string::npos);
  }
}
