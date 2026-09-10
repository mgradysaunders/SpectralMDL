#include "Layout/CameraFile.h"

#include "Layout/Layout.h"
#include "Layout/TextParser.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include <array>
#include <cmath>
#include <filesystem>

// The camera format's own vocabulary, over the syntax core in
// `TextParser.h`. One directive and nothing else: what the picture is
// taken with. A '.response' file is the camera's `response` block
// hoisted to the top level of its own file, so the one parser serves
// both, told at construction which keyword it synchronizes at.

namespace {

// The top-level keywords, which are the synchronization points: the
// camera file's, and the response file's.
constexpr std::array<std::string_view, 1> CAMERA_KEYWORDS{"camera"};
constexpr std::array<std::string_view, 1> RESPONSE_KEYWORDS{"response"};

class Parser final : public TextParser {
public:
  Parser(LayoutDiagnostics &diags, const LayoutSource &source,
         CameraDocument &document)
      : TextParser(diags, source, CAMERA_KEYWORDS), mCamera(&document) {}

  Parser(LayoutDiagnostics &diags, const LayoutSource &source,
         ResponseDocument &document)
      : TextParser(diags, source, RESPONSE_KEYWORDS), mResponse(&document) {}

  void parse() {
    while (mToken.kind != Token::END) {
      try {
        parseStatement();
      } catch (const Recover &) {
        synchronize();
      }
    }
  }

private:
  void parseStatement() {
    if (mToken.kind != Token::WORD) {
      mDiags.error(location(), smdl::concat("expected a directive, got ",
                                            smdl::Quoted(mToken.text)));
      throw Recover();
    }
    if (mCamera && mToken.text == "camera") {
      parseCameraBlock();
    } else if (mResponse && mToken.text == "response") {
      parseResponseStatement();
    } else {
      auto &error{
          mDiags.error(location(), smdl::concat("unknown directive ",
                                                smdl::Quoted(mToken.text)))};
      if (mResponse) {
        if (mToken.text == "camera") {
          error.note({}, "a response file holds one 'response' block; where "
                         "the picture is taken from belongs in the '.camera' "
                         "file that names this one");
        } else {
          error.note({}, "a response file holds one 'response' block, whose "
                         "'band' entries are the curves");
        }
      } else if (std::find(TRANSFORM_OPS.begin(), TRANSFORM_OPS.end(),
                           mToken.text) != TRANSFORM_OPS.end()) {
        error.note({}, "a camera is framed by 'look_from' and 'look_to', not "
                       "by transform operations");
      } else if (mToken.text == "time") {
        error.note({}, "the clock is not the camera's: '-time' names the "
                       "instant, and 'shutter' inside the 'camera' block "
                       "says how long it stays open");
      } else if (mToken.text == "response") {
        error.note({}, "the response belongs inside the 'camera' block, or "
                       "in a '.response' file the camera names");
      } else {
        error.note({}, "a camera file holds one 'camera' block; everything "
                       "about the scene belongs in the layout");
      }
      throw Recover();
    }
  }

  // A `camera { ... }` block. Last one wins per field within the file; a
  // field no directive names is left unset for the command line, or
  // failing that the built-in default, to fill in.
  void parseCameraBlock() {
    if (!mCamera->cameraLoc) mCamera->cameraLoc = location();
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'camera'");
      throw Recover();
    }
    auto &camera{mCamera->camera};
    parseSettings("a camera setting", [&](const std::string &key,
                                          const LayoutLocation &keyLoc) {
      if (key == "look_from") {
        auto v{numbers<3>()};
        camera.lookFrom = float3(v[0], v[1], v[2]);
      } else if (key == "look_to") {
        auto v{numbers<3>()};
        camera.lookTo = float3(v[0], v[1], v[2]);
      } else if (key == "look_up") {
        auto v{numbers<3>()};
        camera.lookUp = float3(v[0], v[1], v[2]);
      } else if (key == "fovy") {
        camera.fovYDeg = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "fstop") {
        camera.fStop = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "aperture") {
        camera.aperture = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "focus") {
        camera.focus = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "lens") {
        camera.lens =
            expect(Token::STRING, "a quoted '.lens' path after 'lens'");
      } else if (key == "sensor") {
        auto v{numbers<2>()};
        camera.sensorMM =
            float2(positive(keyLoc, key, v[0]), positive(keyLoc, key, v[1]));
      } else if (key == "blades") {
        camera.blades = int(numbers<1>()[0]);
      } else if (key == "blade_angle") {
        camera.bladeAngleDeg = numbers<1>()[0];
      } else if (key == "distortion_k1") {
        camera.distortionK1 = numbers<1>()[0];
      } else if (key == "distortion_k2") {
        camera.distortionK2 = numbers<1>()[0];
      } else if (key == "distortion_fit") {
        // A bare keyword, since the flag it mirrors takes no value
        // either.
        camera.shouldFitDistortion = true;
      } else if (key == "vignetting") {
        camera.vignetting = numbers<1>()[0];
      } else if (key == "cat_eye") {
        camera.catEye = numbers<1>()[0];
      } else if (key == "cat_eye_radius") {
        camera.catEyeRadius = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "shutter") {
        const auto value{finite(keyLoc, key, numbers<1>()[0])};
        if (!(value >= 0)) {
          mDiags.error(keyLoc, "expected a nonnegative number for 'shutter' "
                               "(0 or omitted is a shut shutter)");
          throw Recover();
        }
        camera.shutter = value;
      } else if (key == "readout") {
        const auto value{finite(keyLoc, key, numbers<1>()[0])};
        if (!(value >= 0)) {
          mDiags.error(keyLoc, "expected a nonnegative number for 'readout' "
                               "(0 or omitted is a global shutter)");
          throw Recover();
        }
        camera.readout = value;
      } else if (key == "readout_direction") {
        const auto word{
            expect(Token::WORD, "a direction after 'readout_direction'")};
        if (word == "down") {
          camera.readoutDirection = ReadoutDirection::DOWN;
        } else if (word == "up") {
          camera.readoutDirection = ReadoutDirection::UP;
        } else if (word == "left") {
          camera.readoutDirection = ReadoutDirection::LEFT;
        } else if (word == "right") {
          camera.readoutDirection = ReadoutDirection::RIGHT;
        } else {
          mDiags.error(keyLoc,
                       smdl::concat("unknown readout direction ",
                                    smdl::Quoted(word),
                                    " (expected down, up, left, or right)"));
          throw Recover();
        }
      } else if (key == "response") {
        parseResponseSetting(camera, keyLoc);
      } else if (key == "resolution") {
        mDiags
            .error(keyLoc, "'resolution' is a fact about this render, not "
                           "about the camera, so it is not in the file")
            .note({}, "give it with '-resolution', and a sub-rectangle of it "
                      "with '-crop-window'");
        throw Recover();
      } else if (key == "motion") {
        parseCameraMotion(camera, keyLoc);
      } else {
        mDiags.error(
            keyLoc,
            smdl::concat("unknown camera setting ", smdl::Quoted(key),
                         " (expected look_from, look_to, look_up, fovy, "
                         "shutter, readout, readout_direction, lens, sensor, "
                         "response, fstop, aperture, focus, "
                         "blades, blade_angle, distortion_k1, distortion_k2, "
                         "distortion_fit, vignetting, cat_eye, "
                         "cat_eye_radius, or motion)"));
        throw Recover();
      }
    });
  }

  // The `motion { ... }` block inside `camera`: a run of `at <seconds>`
  // keys, each restating whatever settings it names. One block per
  // file; a second is an error rather than a merge, since the keys are
  // one track.
  void parseCameraMotion(CameraSettings &camera, const LayoutLocation &opLoc) {
    if (!camera.motion.empty()) {
      mDiags.error(opLoc, "'motion' appears twice in one camera");
      throw Recover();
    }
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'motion'");
      throw Recover();
    }
    advance(); // '{'
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind != Token::WORD) {
        mDiags.error(location(), "expected 'at' or a camera setting");
        throw Recover();
      }
      const auto word{mToken.text};
      const auto wordLoc{location()};
      advance();
      if (word == "at") {
        const auto time{finite(wordLoc, "at", numbers<1>()[0])};
        if (!camera.motion.empty() && !(time > camera.motion.back().time)) {
          mDiags.error(wordLoc, "the keys of a 'motion' block are written in "
                                "ascending time");
          throw Recover();
        }
        camera.motion.emplace_back().time = time;
        continue;
      }
      if (camera.motion.empty()) {
        mDiags
            .error(wordLoc, "a 'motion' block holds 'at <seconds>' keys, and "
                            "every setting belongs to the key above it")
            .note({}, "write 'motion { at 0 look_from ... at 1 look_from "
                      "... }'");
        throw Recover();
      }
      parseCameraKeySetting(camera.motion.back(), word, wordLoc);
    }
    advance(); // '}'
    if (camera.motion.empty()) {
      mDiags.error(opLoc, "a 'motion' block holds at least one 'at <seconds>' "
                          "key");
      throw Recover();
    }
  }

  // One setting inside a `motion` key. The ones the camera cannot
  // interpolate are named as such rather than as unknown words.
  void parseCameraKeySetting(CameraKey &key, const std::string &setting,
                             const LayoutLocation &settingLoc) {
    if (setting == "look_from") {
      auto v{numbers<3>()};
      key.lookFrom = float3(v[0], v[1], v[2]);
    } else if (setting == "look_to") {
      auto v{numbers<3>()};
      key.lookTo = float3(v[0], v[1], v[2]);
    } else if (setting == "look_up") {
      auto v{numbers<3>()};
      key.lookUp = float3(v[0], v[1], v[2]);
    } else if (setting == "fovy") {
      key.fovYDeg = positive(settingLoc, setting, numbers<1>()[0]);
    } else if (setting == "fstop") {
      key.fStop = positive(settingLoc, setting, numbers<1>()[0]);
    } else if (setting == "aperture") {
      key.aperture = positive(settingLoc, setting, numbers<1>()[0]);
    } else if (setting == "focus") {
      key.focus = positive(settingLoc, setting, numbers<1>()[0]);
    } else if (setting == "blade_angle") {
      key.bladeAngleDeg = numbers<1>()[0];
    } else if (setting == "distortion_k1") {
      key.distortionK1 = numbers<1>()[0];
    } else if (setting == "distortion_k2") {
      key.distortionK2 = numbers<1>()[0];
    } else if (setting == "vignetting") {
      key.vignetting = numbers<1>()[0];
    } else if (setting == "cat_eye") {
      key.catEye = numbers<1>()[0];
    } else if (setting == "cat_eye_radius") {
      key.catEyeRadius = positive(settingLoc, setting, numbers<1>()[0]);
    } else if (setting == "blades" || setting == "distortion_fit" ||
               setting == "shutter" || setting == "readout" ||
               setting == "readout_direction" || setting == "resolution" ||
               setting == "lens" || setting == "sensor" ||
               setting == "response") {
      mDiags
          .error(settingLoc,
                 smdl::concat(smdl::Quoted(setting),
                              " is not a quantity to interpolate, so it "
                              "cannot be keyed"))
          .note({}, "state it once in the 'camera' block instead");
      throw Recover();
    } else {
      mDiags.error(settingLoc,
                   smdl::concat("unknown camera setting ",
                                smdl::Quoted(setting), " in a 'motion' key"));
      throw Recover();
    }
  }

  // The `response` setting inside `camera`: the block itself, or the
  // quoted path of a file holding it. One per camera in either form; a
  // second is an error rather than a merge, since two sets of bands have
  // no meaningful union.
  void parseResponseSetting(CameraSettings &camera,
                            const LayoutLocation &keyLoc) {
    if (mResponseLoc) {
      mDiags
          .error(keyLoc, "a camera reads through one response, and this is "
                         "the second 'response'")
          .note(mResponseLoc, "the first one is here");
      throw Recover();
    }
    mResponseLoc = keyLoc;
    if (mToken.kind == Token::STRING) {
      camera.responseFile = mToken.text;
      advance();
    } else if (mToken.kind == Token::OPEN) {
      camera.response.emplace();
      parseResponseBlock(*camera.response, keyLoc);
    } else {
      mDiags.error(location(), "expected '{' or a quoted '.response' path "
                               "after 'response'");
      throw Recover();
    }
  }

  // The `response` directive of a response file, which is the whole
  // file. As with a lens, a second block does not merge.
  void parseResponseStatement() {
    if (mResponse->responseLoc) {
      mDiags
          .error(location(), "a response file describes one response, and "
                             "this is the second 'response' block")
          .note(mResponse->responseLoc, "the first one is here");
      throw Recover();
    }
    mResponse->responseLoc = location();
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'response'");
      throw Recover();
    }
    parseResponseBlock(mResponse->response, mResponse->responseLoc);
  }

  // The `{ ... }` body of a response, in either place. The bands are
  // read in file order, and the tile's names resolve once the block
  // closes, so a row may name a band declared below it. `{` is current.
  void parseResponseBlock(ResponseSettings &response,
                          const LayoutLocation &responseLoc) {
    // The tile's names as written, with where each was written, for the
    // resolution below.
    auto tileNames{std::vector<std::string>()};
    auto tileLocs{std::vector<LayoutLocation>()};
    auto cfaLoc{LayoutLocation()};
    parseSettings("a response setting", [&](const std::string &key,
                                            const LayoutLocation &keyLoc) {
      if (key == "name") {
        response.name = expect(Token::STRING, "a quoted name after 'name'");
      } else if (key == "kind") {
        const auto word{expect(Token::WORD, "'relative' or 'qe' after 'kind'")};
        if (word == "relative") {
          response.kind = ResponseKind::RELATIVE;
        } else if (word == "qe") {
          response.kind = ResponseKind::QE;
        } else {
          mDiags.error(keyLoc, smdl::concat("unknown response kind ",
                                            smdl::Quoted(word),
                                            " (expected relative or qe)"));
          throw Recover();
        }
      } else if (key == "band") {
        response.bands.push_back(parseResponseBand(response, keyLoc));
      } else if (key == "cfa") {
        if (cfaLoc) {
          mDiags
              .error(keyLoc, "a response lays one tile over the pixels, and "
                             "this is the second 'cfa'")
              .note(cfaLoc, "the first one is here");
          throw Recover();
        }
        cfaLoc = keyLoc;
        parseResponseCFA(response, keyLoc, tileNames, tileLocs);
      } else {
        mDiags.error(keyLoc, smdl::concat("unknown response setting ",
                                          smdl::Quoted(key),
                                          " (expected name, kind, band, or "
                                          "cfa)"));
        throw Recover();
      }
    });
    if (response.bands.empty()) {
      mDiags.error(responseLoc, "a response needs at least one 'band'");
      throw Recover();
    }
    for (size_t i = 0; i < tileNames.size(); i++) {
      const auto index{response.bandIndex(tileNames[i])};
      if (!index) {
        mDiags.error(tileLocs[i],
                     smdl::concat("the tile names ", smdl::Quoted(tileNames[i]),
                                  ", which is not a band of this response"));
        throw Recover();
      }
      response.cfa.push_back(*index);
    }
  }

  // One `band NAME { w v w v ... }`: the name, then the knots as bare
  // number pairs to the closing brace. A malformed knot list is reported
  // against the band, since the numbers are gone by the time it is
  // checked.
  [[nodiscard]] ResponseBand parseResponseBand(const ResponseSettings &response,
                                               const LayoutLocation &bandLoc) {
    if (mToken.kind != Token::WORD || !isIdentifier(mToken.text) ||
        mToken.text == "row") {
      mDiags.error(location(), "expected a band name after 'band': a word "
                               "beginning with a letter or an underscore, "
                               "other than 'row', which is the tile's own");
      throw Recover();
    }
    auto band{ResponseBand{}};
    band.name = mToken.text;
    if (response.bandIndex(band.name)) {
      mDiags.error(location(), smdl::concat("band ", smdl::Quoted(band.name),
                                            " is declared twice"));
      throw Recover();
    }
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(),
                   smdl::concat("expected '{' after 'band ", band.name, "'"));
      throw Recover();
    }
    advance(); // '{'
    auto values{std::vector<float>()};
    for (float value{}; mToken.kind != Token::CLOSE;) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind != Token::WORD || !tryNumber(mToken, value)) {
        mDiags.error(location(), smdl::concat("expected a number or '}' in "
                                              "band ",
                                              smdl::Quoted(band.name), ", got ",
                                              smdl::Quoted(mToken.text)));
        throw Recover();
      }
      values.push_back(value);
      advance();
    }
    advance(); // '}'
    if (values.size() % 2 != 0) {
      mDiags.error(bandLoc, smdl::concat("expected wavelength and value pairs "
                                         "in band ",
                                         smdl::Quoted(band.name), ", got ",
                                         values.size(), " number(s)"));
      throw Recover();
    }
    if (values.size() < 4) {
      mDiags.error(bandLoc, smdl::concat("expected at least two pairs in band ",
                                         smdl::Quoted(band.name),
                                         " (a curve needs two knots)"));
      throw Recover();
    }
    for (size_t i = 0; i < values.size(); i += 2) {
      const float wavelength{values[i]};
      const float value{values[i + 1]};
      if (!(std::isfinite(wavelength) && wavelength > 0)) {
        mDiags.error(bandLoc, smdl::concat("expected a positive wavelength in "
                                           "nanometers in band ",
                                           smdl::Quoted(band.name)));
        throw Recover();
      }
      if (i > 0 && !(wavelength > band.wavelengths.back())) {
        mDiags.error(bandLoc,
                     smdl::concat("expected ascending wavelengths in band ",
                                  smdl::Quoted(band.name), " (",
                                  smdl::Brief(wavelength, 6), " nm follows ",
                                  smdl::Brief(band.wavelengths.back(), 6),
                                  ")"));
        throw Recover();
      }
      if (!(std::isfinite(value) && value >= 0)) {
        mDiags.error(bandLoc,
                     smdl::concat("expected a finite nonnegative "
                                  "value at ",
                                  smdl::Brief(wavelength, 6), " nm in band ",
                                  smdl::Quoted(band.name)));
        throw Recover();
      }
      band.wavelengths.push_back(wavelength);
      band.values.push_back(value);
    }
    return band;
  }

  // The `cfa { row A B  row C D }` tile: rows of band names, all the same
  // length, kept as names until the block closes. `{` is checked here,
  // since the block is not optional once 'cfa' is written.
  void parseResponseCFA(ResponseSettings &response,
                        const LayoutLocation &cfaLoc,
                        std::vector<std::string> &names,
                        std::vector<LayoutLocation> &nameLocs) {
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'cfa'");
      throw Recover();
    }
    size_t numRows{};
    parseSettings("a 'row' of the tile", [&](const std::string &key,
                                             const LayoutLocation &keyLoc) {
      if (key != "row") {
        mDiags.error(keyLoc, smdl::concat("expected 'row' in 'cfa', got ",
                                          smdl::Quoted(key)));
        throw Recover();
      }
      size_t numColumns{};
      while (mToken.kind == Token::WORD && mToken.text != "row") {
        if (!isIdentifier(mToken.text)) {
          mDiags.error(location(), smdl::concat("expected a band name in the "
                                                "row, got ",
                                                smdl::Quoted(mToken.text)));
          throw Recover();
        }
        names.push_back(mToken.text);
        nameLocs.push_back(location());
        numColumns++;
        advance();
      }
      if (numColumns == 0) {
        mDiags.error(keyLoc, "expected at least one band name after 'row'");
        throw Recover();
      }
      if (numRows == 0) {
        response.cfaColumns = numColumns;
      } else if (numColumns != response.cfaColumns) {
        mDiags.error(keyLoc, smdl::concat("expected ", response.cfaColumns,
                                          " band name(s) in this row, as in "
                                          "the first, got ",
                                          numColumns));
        throw Recover();
      }
      numRows++;
    });
    if (numRows == 0) {
      mDiags.error(cfaLoc, "expected at least one 'row' in 'cfa'");
      throw Recover();
    }
  }

  // Exactly one of the two is set, by which constructor ran.
  CameraDocument *mCamera{};
  ResponseDocument *mResponse{};

  // Where the camera's 'response' was written, in either form, so that a
  // second one can point at it.
  LayoutLocation mResponseLoc{};
};

} // namespace

// The framing, which is what the renderer interpolates within one
// shutter, and the rest, which it holds at the shutter's open value.
// Both lists are walked by the key resolution; only the second is
// reported as held.
#define CAMERA_FRAMING_SETTINGS(X) \
  X(lookFrom, "look_from")         \
  X(lookTo, "look_to")             \
  X(lookUp, "look_up")

#define CAMERA_HELD_SETTINGS(X)    \
  X(fovYDeg, "fovy")               \
  X(fStop, "fstop")                \
  X(aperture, "aperture")          \
  X(focus, "focus")                \
  X(bladeAngleDeg, "blade_angle")  \
  X(distortionK1, "distortion_k1") \
  X(distortionK2, "distortion_k2") \
  X(vignetting, "vignetting")      \
  X(catEye, "cat_eye")             \
  X(catEyeRadius, "cat_eye_radius")

namespace {

// One keyable setting at `seconds`: interpolated between the two keys
// that surround it among those that state it, clamped to the outermost
// of them, and `base` when no key states it at all. A key's own time
// returns that key's value exactly.
template <typename T>
[[nodiscard]] std::optional<T>
sampleKeyed(const std::vector<CameraKey> &keys,
            std::optional<T> CameraKeyable::*member,
            const std::optional<T> &base, float seconds) {
  const CameraKey *lo{};
  const CameraKey *hi{};
  for (const auto &key : keys) {
    if (!(key.*member)) continue;
    if (key.time <= seconds) {
      lo = &key;
    } else if (!hi) {
      hi = &key;
    }
  }
  if (!lo && !hi) return base;
  if (!lo) return *(hi->*member);
  if (!hi) return *(lo->*member);
  const float span{hi->time - lo->time};
  const float t{span > 0 ? (seconds - lo->time) / span : 0.0f};
  return T((1.0f - t) * *(lo->*member) + t * *(hi->*member));
}

[[nodiscard]] bool differs(const std::optional<float> &a,
                           const std::optional<float> &b) {
  return a.has_value() != b.has_value() || (a && *a != *b);
}

} // namespace

std::optional<size_t>
ResponseSettings::bandIndex(std::string_view name) const noexcept {
  for (size_t i = 0; i < bands.size(); i++)
    if (bands[i].name == name) return i;
  return std::nullopt;
}

CameraSettings CameraSettings::at(float seconds) const {
  auto result{*this};
  result.motion.clear();
  if (motion.empty()) return result;
#define X(member, name) \
  result.member = sampleKeyed(motion, &CameraKeyable::member, member, seconds);
  CAMERA_FRAMING_SETTINGS(X)
  CAMERA_HELD_SETTINGS(X)
#undef X
  return result;
}

bool CameraSettings::hasKeyBetween(float open, float shut) const {
  for (const auto &key : motion)
    if (key.time > open && key.time < shut) return true;
  return false;
}

std::vector<std::string_view>
CameraSettings::heldOverShutter(float open, float shut) const {
  auto held{std::vector<std::string_view>()};
  if (motion.empty()) return held;
  const auto a{at(open)};
  const auto b{at(shut)};
#define X(member, name) \
  if (differs(a.member, b.member)) held.push_back(name);
  CAMERA_HELD_SETTINGS(X)
#undef X
  return held;
}

CameraDocument parseCamera(LayoutDiagnostics &diags,
                           const LayoutSource &source) {
  auto document{CameraDocument()};
  document.source = &source;
  Parser(diags, source, document).parse();
  return document;
}

CameraDocument readCamera(const std::string &fileName) {
  auto diags{LayoutDiagnostics()};
  const auto &source{diags.loadSource(fileName)};
  auto document{parseCamera(diags, source)};
  if (!diags.empty()) diags.printAll(smdl::cerrSupportsANSIColors());
  if (diags.hasErrors())
    throw smdl::Error(smdl::concat("cannot read ", smdl::QuotedPath(fileName),
                                   ": ", diags.summary()));
  SMDL_LOG_DEBUG("Read ", smdl::QuotedPath(fileName));
  return document;
}

std::string resolveCameraFileName(const std::string &given,
                                  const std::string &sceneFileName) {
  if (!given.empty()) {
    if (!std::filesystem::exists(given))
      throw smdl::Error(
          smdl::concat("-camera ", smdl::QuotedPath(given), " does not exist"));
    return given;
  }
  if (sceneFileName.empty()) return {};
  auto path{std::filesystem::path(sceneFileName)};
  if (path.extension() != LAYOUT_EXTENSION) return {};
  path.replace_extension(CAMERA_EXTENSION);
  if (!std::filesystem::exists(path)) return {};
  return path.string();
}

ResponseDocument parseResponse(LayoutDiagnostics &diags,
                               const LayoutSource &source) {
  auto document{ResponseDocument()};
  document.source = &source;
  Parser(diags, source, document).parse();
  // A camera named this file for its bands, so a file with none is an
  // error rather than an empty response; the caret sits at the start,
  // there being nothing else to point at.
  if (!document.responseLoc && !diags.hasErrors())
    diags.error(LayoutLocation{&source, 0, 1},
                "a response file holds one 'response' block, and this one "
                "has none");
  return document;
}

ResponseDocument readResponse(const std::string &fileName) {
  auto diags{LayoutDiagnostics()};
  const auto &source{diags.loadSource(fileName)};
  auto document{parseResponse(diags, source)};
  if (!diags.empty()) diags.printAll(smdl::cerrSupportsANSIColors());
  if (diags.hasErrors())
    throw smdl::Error(smdl::concat("cannot read ", smdl::QuotedPath(fileName),
                                   ": ", diags.summary()));
  SMDL_LOG_DEBUG("Read ", smdl::QuotedPath(fileName));
  return document;
}

std::string resolveResponseFileName(const std::string &given,
                                    const std::string &cameraFileName,
                                    const std::string &stated) {
  if (!given.empty()) {
    if (!std::filesystem::exists(given))
      throw smdl::Error(smdl::concat("-response ", smdl::QuotedPath(given),
                                     " does not exist"));
    return given;
  }
  if (stated.empty()) return {};
  auto path{std::filesystem::path(stated)};
  if (path.is_relative() && !cameraFileName.empty())
    path = std::filesystem::path(cameraFileName).parent_path() / path;
  if (!std::filesystem::exists(path))
    throw smdl::Error(smdl::concat("the camera file names the response ",
                                   smdl::QuotedPath(stated),
                                   ", which does not exist beside it"));
  return path.string();
}
