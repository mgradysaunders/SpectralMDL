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
// taken with. The body and the lens are files it names, so the words it
// knows are the shot's alone.

namespace {

// The top-level keywords, which are the synchronization points.
constexpr std::array<std::string_view, 1> TOP_LEVEL_KEYWORDS{"camera"};

class Parser final : public TextParser {
public:
  Parser(LayoutDiagnostics &diags, const LayoutSource &source,
         CameraDocument &document)
      : TextParser(diags, source, TOP_LEVEL_KEYWORDS), mDocument(document) {}

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
    if (mToken.text == "camera") {
      parseCameraBlock();
    } else {
      auto &error{
          mDiags.error(location(), smdl::concat("unknown directive ",
                                                smdl::Quoted(mToken.text)))};
      if (std::find(TRANSFORM_OPS.begin(), TRANSFORM_OPS.end(), mToken.text) !=
          TRANSFORM_OPS.end()) {
        error.note({}, "a camera is framed by 'look_from' and 'look_to', not "
                       "by transform operations");
      } else if (mToken.text == "time") {
        error.note({}, "the clock is not the camera's: '-time' names the "
                       "instant, and 'shutter' inside the 'camera' block "
                       "says how long it stays open");
      } else if (mToken.text == "response" || mToken.text == "detector") {
        error.note({}, smdl::concat("the ", mToken.text,
                                    " is the body's: it is a block inside "
                                    "the 'sensor' block of the '.sensor' "
                                    "file the camera names"));
      } else if (mToken.text == "sensor") {
        error.note({}, "'sensor' is a setting inside the 'camera' block, "
                       "naming the '.sensor' file the picture lands on");
      } else {
        error.note({}, "a camera file holds one 'camera' block; everything "
                       "about the scene belongs in the layout");
      }
      throw Recover();
    }
  }

  // The keys whose meaning moved into the sensor file, each refused
  // with a note saying where it went, in the block and in a key alike.
  // Returns false if `key` is not one of them.
  [[nodiscard]] bool refuseMovedKey(const std::string &key,
                                    const LayoutLocation &keyLoc) {
    if (key == "response" || key == "detector") {
      mDiags
          .error(keyLoc,
                 smdl::concat("'", key, "' is no longer a camera setting"))
          .note({}, smdl::concat("it is the '", key,
                                 "' block of the '.sensor' file the camera "
                                 "names with 'sensor'"));
      return true;
    }
    return false;
  }

  // A `camera { ... }` block. Last one wins per field within the file; a
  // field no directive names is left unset for the command line, or
  // failing that the built-in default, to fill in.
  void parseCameraBlock() {
    if (!mDocument.cameraLoc) mDocument.cameraLoc = location();
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'camera'");
      throw Recover();
    }
    auto &camera{mDocument.camera};
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
        camera.lens = parseFileOrWord(key, LENS_IDEAL, "'.lens'");
      } else if (key == "sensor") {
        parseSensorSetting(camera, keyLoc);
      } else if (key == "temperature") {
        camera.temperature = finite(keyLoc, key, numbers<1>()[0]);
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
      } else if (refuseMovedKey(key, keyLoc)) {
        throw Recover();
      } else if (key == "resolution") {
        mDiags
            .error(keyLoc, "'resolution' is a fact about this render, not "
                           "about the camera, so it is not in the file")
            .note({}, "give it with '-resolution', and a sub-rectangle of it "
                      "with '-crop-window'; a physical sensor's pixels are "
                      "its own");
        throw Recover();
      } else if (key == "motion") {
        parseCameraMotion(camera, keyLoc);
      } else {
        mDiags.error(
            keyLoc,
            smdl::concat("unknown camera setting ", smdl::Quoted(key),
                         " (expected look_from, look_to, look_up, fovy, "
                         "shutter, readout, readout_direction, lens, sensor, "
                         "temperature, fstop, aperture, focus, blades, "
                         "blade_angle, distortion_k1, distortion_k2, "
                         "distortion_fit, vignetting, cat_eye, "
                         "cat_eye_radius, or motion)"));
        throw Recover();
      }
    });
  }

  // A quoted path, or the one bare word that stands in for a file:
  // `lens "x.lens"` or `lens ideal`. The word is kept as the value, so a
  // reader tells the two apart by whether it is that word.
  [[nodiscard]] std::string parseFileOrWord(const std::string &key,
                                            std::string_view word,
                                            const char *extension) {
    if (mToken.kind == Token::STRING) {
      auto path{mToken.text};
      advance();
      return path;
    }
    if (mToken.kind == Token::WORD && mToken.text == word) {
      advance();
      return std::string(word);
    }
    mDiags.error(location(),
                 smdl::concat("expected a quoted ", extension, " path or '",
                              word, "' after '", key, "'"));
    throw Recover();
  }

  // The `sensor` setting: a path or `human`. The two old spellings, a
  // width and a height and an inline block, each say where their meaning
  // went.
  void parseSensorSetting(CameraSettings &camera,
                          const LayoutLocation &keyLoc) {
    if (mToken.kind == Token::OPEN) {
      mDiags.error(keyLoc, "'sensor' names a body, and a body is a file")
          .note({}, "write the block in a '.sensor' file and name it here: "
                    "sensor \"body.sensor\"");
      throw Recover();
    }
    if (mToken.kind == Token::WORD && isNumber(mToken)) {
      mDiags.error(keyLoc, "'sensor' no longer takes a width and a height")
          .note({}, "the thin lens spans a frame 24 mm tall whose width "
                    "follows the picture; a physical frame is a '.sensor' "
                    "file's 'pixels' and 'pitch', and 'sensor "
                    "\"body.sensor\"' names it");
      throw Recover();
    }
    camera.sensor = parseFileOrWord("sensor", SENSOR_HUMAN, "'.sensor'");
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
    } else if (refuseMovedKey(setting, settingLoc)) {
      throw Recover();
    } else if (setting == "blades" || setting == "distortion_fit" ||
               setting == "shutter" || setting == "readout" ||
               setting == "readout_direction" || setting == "resolution" ||
               setting == "lens" || setting == "sensor" ||
               setting == "temperature") {
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

  CameraDocument &mDocument;
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
