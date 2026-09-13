#include "Layout/SensorFile.h"

#include "Layout/TextParser.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <filesystem>

// The sensor format's own vocabulary, over the syntax core in
// `TextParser.h`. One directive and nothing else: what the picture lands
// on.

namespace {

// The top-level keywords, which are the synchronization points.
constexpr std::array<std::string_view, 1> TOP_LEVEL_KEYWORDS{"sensor"};

// The extension of the response sidecar this format replaced, so that a
// path naming one gets a note saying where its meaning went rather than
// a parse error on its first word.
constexpr std::string_view RESPONSE_EXTENSION = ".response";

// Pitches derived from `size` that differ by more than this fraction are
// refused: a sensor's pixels are square, and a size that disagrees by more
// is a typo or a crop.
constexpr float PITCH_TOLERANCE = 0.005f;

// The largest leak every band could take alike and still be one these
// curves carry, by bisection on `crosstalkFloor()`. Only ever asked on
// the way to refusing a leak, so the copy it takes costs nothing.
[[nodiscard]] float largestUniformLeak(const ResponseSettings &settings) {
  ResponseSettings probe{settings};
  float lo{0.0f};
  float hi{CROSSTALK_MAX_LEAK};
  for (int i = 0; i < 24; i++) {
    const float mid{0.5f * (lo + hi)};
    probe.crosstalk.assign(settings.bands.size(), mid);
    if (crosstalkFloor(probe) >= -CROSSTALK_NEGATIVE_TOLERANCE)
      lo = mid;
    else
      hi = mid;
  }
  return lo;
}

class Parser final : public TextParser {
public:
  Parser(LayoutDiagnostics &diags, const LayoutSource &source,
         SensorDocument &document)
      : TextParser(diags, source, TOP_LEVEL_KEYWORDS), mDocument(document) {}

  void parse() {
    parseStatements([this] { parseStatement(); });
  }

private:
  void parseStatement() {
    if (mToken.kind != Token::WORD) {
      mDiags.error(location(), smdl::concat("expected a directive, got ",
                                            smdl::Quoted(mToken.text)));
      throw Recover();
    }
    if (mToken.text == "sensor") {
      parseSensorBlock();
    } else {
      LayoutDiagnostic &error{
          mDiags.error(location(), smdl::concat("unknown directive ",
                                                smdl::Quoted(mToken.text)))};
      if (mToken.text == "camera" || mToken.text == "lens") {
        error.note({}, "a sensor file describes the sensor alone; where the "
                       "picture is taken from and through what belongs in "
                       "the '.camera' file that names this one");
      } else if (mToken.text == "response" || mToken.text == "detector") {
        error.note({}, smdl::concat(smdl::Quoted(mToken.text),
                                    " is a block inside 'sensor', not a "
                                    "directive of its own"));
      } else {
        error.note({}, "a sensor file holds one 'sensor' block: the pixels, "
                       "the pitch, the response, the detector, and the "
                       "readout");
      }
      throw Recover();
    }
  }

  // A `sensor { ... }` block, which is the whole file. A second one does
  // not merge: a sensor is one thing, and two have no meaningful union.
  void parseSensorBlock() {
    if (mDocument.sensorLoc) {
      mDiags
          .error(location(), "a sensor file describes one sensor, and this is "
                             "the second 'sensor' block")
          .note(mDocument.sensorLoc, "the first one is here");
      throw Recover();
    }
    mDocument.sensorLoc = location();
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'sensor'");
      throw Recover();
    }
    SensorSettings &sensor{mDocument.sensor};
    LayoutLocation pixelsLoc{};
    LayoutLocation pitchLoc{};
    LayoutLocation sizeLoc{};
    LayoutLocation responseLoc{};
    LayoutLocation detectorLoc{};
    float2 sizeMM{};
    parseSettings("a sensor setting", [&](const std::string &key,
                                          const LayoutLocation &keyLoc) {
      if (key == "name") {
        sensor.name = expect(Token::STRING, "a quoted name after 'name'");
      } else if (key == "pixels") {
        pixelsLoc = keyLoc;
        const std::array<float, 2> v{numbers<2>()};
        for (const auto value : v) {
          if (!(value >= 1 && value == std::floor(value) && value < 1e9f)) {
            mDiags.error(keyLoc, "expected two positive integers for "
                                 "'pixels' (columns and rows)");
            throw Recover();
          }
        }
        sensor.pixels = int2(int(v[0]), int(v[1]));
      } else if (key == "pitch") {
        pitchLoc = keyLoc;
        // One number for square pixels, two for the rare rectangular
        // ones, so the count is read off the tokens.
        float across{0.0f};
        if (mToken.kind != Token::WORD || !tryNumber(mToken, across)) {
          mDiags.error(location(), "expected one or two positive numbers in "
                                   "micrometers after 'pitch'");
          throw Recover();
        }
        advance();
        float down{across};
        if (mToken.kind == Token::WORD && tryNumber(mToken, down)) advance();
        sensor.pitchUM =
            float2(positive(keyLoc, key, across), positive(keyLoc, key, down));
      } else if (key == "size") {
        sizeLoc = keyLoc;
        const std::array<float, 2> v{numbers<2>()};
        sizeMM =
            float2(positive(keyLoc, key, v[0]), positive(keyLoc, key, v[1]));
      } else if (key == "response") {
        if (responseLoc) {
          mDiags
              .error(keyLoc, "a sensor reads through one response, and this "
                             "is the second 'response'")
              .note(responseLoc, "the first one is here");
          throw Recover();
        }
        responseLoc = keyLoc;
        if (mToken.kind == Token::STRING) {
          mDiags.error(keyLoc, "'response' is a block here, not a path")
              .note({}, "a sensor is one file: write the bands inside "
                        "'response { ... }' beside the pixels and the pitch");
          throw Recover();
        }
        if (mToken.kind != Token::OPEN) {
          mDiags.error(location(), "expected '{' after 'response'");
          throw Recover();
        }
        parseResponseBlock(sensor.response, keyLoc);
      } else if (key == "detector") {
        if (detectorLoc) {
          mDiags
              .error(keyLoc, "a sensor reads out through one detector, and "
                             "this is the second 'detector'")
              .note(detectorLoc, "the first one is here");
          throw Recover();
        }
        detectorLoc = keyLoc;
        if (mToken.kind != Token::OPEN) {
          mDiags.error(location(), "expected '{' after 'detector'");
          throw Recover();
        }
        sensor.hasDetectorBlock = true;
        parseDetectorBlock(sensor.detector);
      } else if (key == "readout") {
        const float value{finite(keyLoc, key, numbers<1>()[0])};
        if (!(value >= 0)) {
          mDiags.error(keyLoc, "expected a nonnegative number for 'readout' "
                               "(0 or omitted is a global shutter)");
          throw Recover();
        }
        sensor.readout = value;
      } else if (key == "readout_direction") {
        const std::string word{
            expect(Token::WORD, "a direction after 'readout_direction'")};
        if (word == "down") {
          sensor.readoutDirection = ReadoutDirection::DOWN;
        } else if (word == "up") {
          sensor.readoutDirection = ReadoutDirection::UP;
        } else if (word == "left") {
          sensor.readoutDirection = ReadoutDirection::LEFT;
        } else if (word == "right") {
          sensor.readoutDirection = ReadoutDirection::RIGHT;
        } else {
          mDiags.error(keyLoc,
                       smdl::concat("unknown readout direction ",
                                    smdl::Quoted(word),
                                    " (expected down, up, left, or right)"));
          throw Recover();
        }
      } else if (key == "temperature") {
        mDiags
            .error(keyLoc, "'temperature' is a condition of the shot, not a "
                           "fact about the sensor")
            .note({}, "state it in the 'camera' block");
        throw Recover();
      } else if (key == "kind" || key == "peak_qe" || key == "band" ||
                 key == "cfa" || key == "rgb" || key == "crosstalk") {
        mDiags.error(keyLoc, smdl::concat(smdl::Quoted(key),
                                          " belongs inside the 'response' "
                                          "block"));
        throw Recover();
      } else if (key == "full_well" || key == "base_iso" ||
                 key == "read_noise" || key == "dark_current" ||
                 key == "black_level" || key == "bits" || key == "gain") {
        mDiags.error(keyLoc, smdl::concat(smdl::Quoted(key),
                                          " belongs inside the 'detector' "
                                          "block"));
        throw Recover();
      } else {
        mDiags.error(
            keyLoc,
            smdl::concat("unknown sensor setting ", smdl::Quoted(key),
                         " (expected name, pixels, pitch, size, response, "
                         "detector, readout, or readout_direction)"));
        throw Recover();
      }
    });
    if (!pixelsLoc) {
      mDiags.error(mDocument.sensorLoc,
                   "a sensor needs 'pixels', its columns and rows");
      throw Recover();
    }
    if (!pitchLoc && !sizeLoc) {
      mDiags.error(mDocument.sensorLoc,
                   "a sensor needs its pitch: 'pitch' in micrometers, or "
                   "'size' in millimeters for the pitch to follow from");
      throw Recover();
    }
    if (pitchLoc && sizeLoc) {
      mDiags
          .error(sizeLoc, "'pitch' and 'size' both say how big a pixel is; "
                          "state one")
          .note(pitchLoc, "'pitch' is here");
      throw Recover();
    }
    if (sizeLoc) {
      const float2 pitch{1e3f * sizeMM.x / float(sensor.pixels.x),
                         1e3f * sizeMM.y / float(sensor.pixels.y)};
      if (std::abs(pitch.x - pitch.y) >
          PITCH_TOLERANCE * std::max(pitch.x, pitch.y)) {
        mDiags
            .error(sizeLoc,
                   smdl::concat(
                       "'size' ", smdl::Brief(sizeMM.x, 6), " by ",
                       smdl::Brief(sizeMM.y, 6), " mm over ", sensor.pixels.x,
                       " by ", sensor.pixels.y, " pixels gives a pitch of ",
                       smdl::Brief(pitch.x, 5), " by ", smdl::Brief(pitch.y, 5),
                       " um, which is not square"))
            .note({}, smdl::concat("state 'pitch ", smdl::Brief(pitch.x, 5),
                                   " ", smdl::Brief(pitch.y, 5),
                                   "' to mean it, or fix the size"));
        throw Recover();
      }
      sensor.pitchUM = pitch;
    }
    if (!responseLoc) {
      mDiags.error(mDocument.sensorLoc, "a sensor needs a 'response' block "
                                        "with at least one 'band'");
      throw Recover();
    }
  }

  // The `{ ... }` body of the response. The bands are read in file
  // order, and the names the tile and `rgb` use resolve once the block
  // closes, so either may name a band declared below it. `{` is current.
  void parseResponseBlock(ResponseSettings &response,
                          const LayoutLocation &responseLoc) {
    std::vector<std::string> tileNames{};
    std::vector<LayoutLocation> tileLocs{};
    std::vector<std::string> rgbNames{};
    std::vector<LayoutLocation> rgbLocs{};
    std::vector<std::string> leakNames{};
    std::vector<float> leakValues{};
    std::vector<LayoutLocation> leakLocs{};
    std::optional<float> leakEveryBand{};
    LayoutLocation cfaLoc{};
    LayoutLocation rgbLoc{};
    LayoutLocation peakLoc{};
    LayoutLocation crosstalkLoc{};
    parseSettings("a response setting", [&](const std::string &key,
                                            const LayoutLocation &keyLoc) {
      if (key == "kind") {
        const std::string word{
            expect(Token::WORD, "'relative' or 'qe' after 'kind'")};
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
      } else if (key == "peak_qe") {
        peakLoc = keyLoc;
        const float value{finite(keyLoc, key, numbers<1>()[0])};
        if (!(value > 0 && value <= 1)) {
          mDiags.error(keyLoc, "expected 'peak_qe' between 0 and 1 "
                               "(electrons per photon at the curve's peak)");
          throw Recover();
        }
        response.peakQE = value;
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
      } else if (key == "rgb") {
        if (rgbLoc) {
          mDiags
              .error(keyLoc, "a response names its three color bands once, "
                             "and this is the second 'rgb'")
              .note(rgbLoc, "the first one is here");
          throw Recover();
        }
        rgbLoc = keyLoc;
        for (int i = 0; i < 3; i++) {
          if (mToken.kind != Token::WORD || !isIdentifier(mToken.text)) {
            mDiags.error(location(), "expected three band names after 'rgb', "
                                     "the ones a develop maps to red, green, "
                                     "and blue");
            throw Recover();
          }
          rgbNames.push_back(mToken.text);
          rgbLocs.push_back(location());
          advance();
        }
      } else if (key == "crosstalk") {
        if (crosstalkLoc) {
          mDiags
              .error(keyLoc, "a response states its cross-talk once, and this "
                             "is the second 'crosstalk'")
              .note(crosstalkLoc, "the first one is here");
          throw Recover();
        }
        crosstalkLoc = keyLoc;
        parseResponseCrosstalk(keyLoc, leakEveryBand, leakNames, leakValues,
                               leakLocs);
      } else if (key == "name") {
        mDiags.error(keyLoc, "'name' is the sensor's, not the response's")
            .note({}, "state it in the 'sensor' block, one level up");
        throw Recover();
      } else {
        mDiags.error(keyLoc, smdl::concat("unknown response setting ",
                                          smdl::Quoted(key),
                                          " (expected kind, peak_qe, band, "
                                          "cfa, rgb, or crosstalk)"));
        throw Recover();
      }
    });
    if (response.bands.empty()) {
      mDiags.error(responseLoc, "a response needs at least one 'band'");
      throw Recover();
    }
    if (peakLoc && response.kind == ResponseKind::QE) {
      mDiags
          .error(peakLoc, "'peak_qe' scales a 'relative' curve, and this "
                          "response is 'qe' already")
          .note({}, "a 'qe' curve is in electrons per photon as written");
      throw Recover();
    }
    for (size_t i = 0; i < tileNames.size(); i++) {
      const std::optional<size_t> index{response.bandIndex(tileNames[i])};
      if (!index) {
        mDiags.error(tileLocs[i],
                     smdl::concat("the tile names ", smdl::Quoted(tileNames[i]),
                                  ", which is not a band of this response"));
        throw Recover();
      }
      response.cfa.push_back(*index);
    }
    if (rgbLoc) {
      std::array<size_t, 3> rgb{};
      for (size_t i = 0; i < 3; i++) {
        const std::optional<size_t> index{response.bandIndex(rgbNames[i])};
        if (!index) {
          mDiags.error(rgbLocs[i],
                       smdl::concat("'rgb' names ", smdl::Quoted(rgbNames[i]),
                                    ", which is not a band of "
                                    "this response"));
          throw Recover();
        }
        rgb[i] = *index;
      }
      response.rgb = rgb;
    }
    if (crosstalkLoc) {
      response.crosstalk.assign(response.bands.size(),
                                leakEveryBand.value_or(0.0f));
      for (size_t i = 0; i < leakNames.size(); i++) {
        const std::optional<size_t> index{response.bandIndex(leakNames[i])};
        if (!index) {
          mDiags.error(leakLocs[i],
                       smdl::concat("'crosstalk' names ",
                                    smdl::Quoted(leakNames[i]),
                                    ", which is not a band of this response"));
          throw Recover();
        }
        for (size_t j = 0; j < i; j++) {
          if (leakNames[j] != leakNames[i]) continue;
          mDiags
              .error(leakLocs[i],
                     smdl::concat("'crosstalk' states band ",
                                  smdl::Quoted(leakNames[i]), " twice"))
              .note(leakLocs[j], "the first one is here");
          throw Recover();
        }
        response.crosstalk[*index] = leakValues[i];
      }
      checkCrosstalk(response, crosstalkLoc);
    }
  }

  // The leaks against the curves. A leak is only ever a fraction of the
  // charge of a pixel the tile lays down, so a response without one has
  // nothing to check: `tileMixing()` is the identity there and the
  // readout's gather is a same-band blur.
  void checkCrosstalk(const ResponseSettings &response,
                      const LayoutLocation &crosstalkLoc) {
    if (!response.hasCrosstalk() || !response.hasCFA()) return;
    if (tileMixingInverse(response).empty()) {
      mDiags.error(crosstalkLoc,
                   "these leaks mix this tile's bands into each other with "
                   "no way back, so no crosstalk-free curve set implies the "
                   "stated one");
      throw Recover();
    }
    const double floor{crosstalkFloor(response)};
    if (floor >= -CROSSTALK_NEGATIVE_TOLERANCE) return;
    mDiags
        .error(crosstalkLoc,
               smdl::concat("this response cannot carry a leak that large: "
                            "the crosstalk-free curves it implies fall to ",
                            smdl::Brief(floor, 3),
                            " of the largest stated value"))
        .note({}, "a measured curve is what the whole array reads at once, "
                  "so it already carries whatever charge crosses between "
                  "pixels; a leak past what it can carry is claiming "
                  "transport for what is the color filter's own "
                  "transmission")
        .note({}, smdl::concat("these curves carry a leak of up to ",
                               smdl::Brief(largestUniformLeak(response), 3),
                               " in every band"));
    throw Recover();
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
    ResponseBand band{};
    band.name = mToken.text;
    if (response.bandIndex(band.name)) {
      mDiags.error(location(), smdl::concat("band ", smdl::Quoted(band.name),
                                            " is declared twice"));
      throw Recover();
    }
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(
          location(),
          smdl::concat("expected '{' after ",
                       smdl::Quoted(smdl::concat("band ", band.name))));
      throw Recover();
    }
    advance(); // '{'
    std::vector<float> values{};
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
      mDiags.error(bandLoc,
                   smdl::concat("expected wavelength and value pairs "
                                "in band ",
                                smdl::Quoted(band.name), ", got ",
                                smdl::Counted(values.size(), "number")));
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

  // The `crosstalk` leak: one number every band takes alike, or a
  // `{ NAME value ... }` block naming bands, whose names are kept until
  // the block closes as the tile's are, so either may name a band
  // declared below it. A band the block leaves out keeps the scalar, or
  // zero where there was none.
  void parseResponseCrosstalk(const LayoutLocation &keyLoc,
                              std::optional<float> &everyBand,
                              std::vector<std::string> &names,
                              std::vector<float> &values,
                              std::vector<LayoutLocation> &nameLocs) {
    if (mToken.kind != Token::OPEN) {
      everyBand = leak(keyLoc, numbers<1>()[0]);
      return;
    }
    parseSettings("a band name", [&](const std::string &name,
                                     const LayoutLocation &nameLoc) {
      if (!isIdentifier(name)) {
        mDiags.error(nameLoc,
                     smdl::concat("expected a band name in 'crosstalk', got ",
                                  smdl::Quoted(name)));
        throw Recover();
      }
      names.push_back(name);
      nameLocs.push_back(nameLoc);
      values.push_back(leak(nameLoc, numbers<1>()[0]));
    });
  }

  // One leak, checked for what the format can check without the curves;
  // what they can carry is `checkCrosstalk()`, once they are all read.
  [[nodiscard]] float leak(const LayoutLocation &keyLoc, float value) {
    if (!(std::isfinite(value) && value >= 0 && value < CROSSTALK_MAX_LEAK)) {
      mDiags.error(keyLoc,
                   smdl::concat("expected a 'crosstalk' leak in [0, ",
                                smdl::Brief(CROSSTALK_MAX_LEAK, 4),
                                "), the fraction of a pixel's charge each "
                                "one of its four neighbors collects"));
      throw Recover();
    }
    return value;
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
        mDiags.error(
            keyLoc,
            smdl::concat("expected ",
                         smdl::Counted(response.cfaColumns, "band name"),
                         " in this row, as in the first, got ", numColumns));
        throw Recover();
      }
      numRows++;
    });
    if (numRows == 0) {
      mDiags.error(cfaLoc, "expected at least one 'row' in 'cfa'");
      throw Recover();
    }
  }

  // The `detector { ... }` block. Every key takes one number and the
  // last one wins. What ties two keys together is checked once the
  // block closes, so the order they are written in does not matter.
  void parseDetectorBlock(DetectorSettings &detector) {
    LayoutLocation baseISOLoc{};
    LayoutLocation fullWellLoc{};
    LayoutLocation blackLevelLoc{};
    LayoutLocation maxISOLoc{};
    parseSettings("a detector setting", [&](const std::string &key,
                                            const LayoutLocation &settingLoc) {
      const auto nonnegative{[&](float value) {
        value = finite(settingLoc, key, value);
        if (!(value >= 0)) {
          mDiags.error(settingLoc,
                       smdl::concat("expected a nonnegative number for ",
                                    smdl::Quoted(key)));
          throw Recover();
        }
        return value;
      }};
      const auto positiveFinite{[&](float value) {
        return positive(settingLoc, key, finite(settingLoc, key, value));
      }};
      if (key == "base_iso") {
        baseISOLoc = settingLoc;
        detector.baseISO = positiveFinite(numbers<1>()[0]);
      } else if (key == "full_well") {
        fullWellLoc = settingLoc;
        detector.fullWell = positiveFinite(numbers<1>()[0]);
      } else if (key == "read_noise") {
        detector.readNoise = nonnegative(numbers<1>()[0]);
      } else if (key == "dark_current") {
        detector.darkCurrent = nonnegative(numbers<1>()[0]);
      } else if (key == "reference_temperature") {
        detector.referenceTemperature =
            finite(settingLoc, key, numbers<1>()[0]);
      } else if (key == "doubling_temperature") {
        detector.doublingTemperature = positiveFinite(numbers<1>()[0]);
      } else if (key == "black_level") {
        blackLevelLoc = settingLoc;
        detector.blackLevel = nonnegative(numbers<1>()[0]);
      } else if (key == "bits") {
        const float value{numbers<1>()[0]};
        if (!(value >= 1 && value <= 16 && value == std::floor(value))) {
          mDiags.error(settingLoc,
                       "expected an integer from 1 to 16 for 'bits'");
          throw Recover();
        }
        detector.bits = int(value);
      } else if (key == "gain") {
        detector.gain = positiveFinite(numbers<1>()[0]);
      } else if (key == "max_iso") {
        maxISOLoc = settingLoc;
        detector.maxISO = positiveFinite(numbers<1>()[0]);
      } else if (key == "temperature") {
        mDiags
            .error(settingLoc, "'temperature' is a condition of the shot, "
                               "not a fact about the detector")
            .note({}, "state it in the 'camera' block");
        throw Recover();
      } else {
        mDiags.error(
            settingLoc,
            smdl::concat("unknown detector setting ", smdl::Quoted(key),
                         " (expected base_iso, full_well, read_noise, "
                         "dark_current, reference_temperature, "
                         "doubling_temperature, black_level, bits, gain, or "
                         "max_iso)"));
        throw Recover();
      }
    });
    if (baseISOLoc && fullWellLoc) {
      mDiags
          .error(fullWellLoc, "'base_iso' and 'full_well' are one fact: the "
                              "base ISO is the lowest at which the ADC "
                              "clips before the well does, so state one")
          .note(baseISOLoc, "'base_iso' is here");
      throw Recover();
    }
    if (blackLevelLoc) {
      if (!(detector.blackLevel < float(detector.topCode()))) {
        mDiags.error(blackLevelLoc,
                     smdl::concat("expected 'black_level' below the top code, "
                                  "which is ",
                                  detector.topCode(), " at ", detector.bits,
                                  " bits"));
        throw Recover();
      }
    } else {
      detector.blackLevel = std::exp2(float(detector.bits - 5));
    }
    if (maxISOLoc && detector.baseISO && detector.maxISO < *detector.baseISO) {
      mDiags.error(maxISOLoc, "expected 'max_iso' to be at least 'base_iso'");
      throw Recover();
    }
  }

  SensorDocument &mDocument;
};

} // namespace

std::optional<size_t>
ResponseSettings::bandIndex(std::string_view name) const noexcept {
  for (size_t i = 0; i < bands.size(); i++)
    if (bands[i].name == name) return i;
  return std::nullopt;
}

std::optional<std::array<size_t, 3>>
ResponseSettings::rgbBands() const noexcept {
  if (rgb) return rgb;
  const std::optional<size_t> r{bandIndex("R")};
  const std::optional<size_t> g{bandIndex("G")};
  const std::optional<size_t> b{bandIndex("B")};
  if (r && g && b) return std::array<size_t, 3>{*r, *g, *b};
  if (bands.size() >= 3) return std::array<size_t, 3>{0, 1, 2};
  return std::nullopt;
}

std::vector<double> tileMixing(const ResponseSettings &settings) {
  const size_t numBands{settings.bands.size()};
  std::vector<double> mixing(numBands * numBands, 0.0);
  for (size_t b = 0; b < numBands; b++) mixing[b * numBands + b] = 1.0;
  if (!settings.hasCFA() || !settings.hasCrosstalk()) return mixing;
  SMDL_SANITY_CHECK(settings.crosstalk.size() == numBands);
  const size_t columns{settings.cfaColumns};
  const size_t rows{settings.cfaRows()};
  std::vector<size_t> cellCount(numBands, 0);
  std::vector<double> neighbors(numBands * numBands, 0.0);
  for (size_t y = 0; y < rows; y++) {
    for (size_t x = 0; x < columns; x++) {
      const size_t b{settings.bandAt(x, y)};
      cellCount[b]++;
      // `bandAt()` takes the tile's own modulus, so adding the period
      // before stepping back is the periodic wrap.
      neighbors[b * numBands + settings.bandAt(x + columns - 1, y)] += 1.0;
      neighbors[b * numBands + settings.bandAt(x + 1, y)] += 1.0;
      neighbors[b * numBands + settings.bandAt(x, y + rows - 1)] += 1.0;
      neighbors[b * numBands + settings.bandAt(x, y + 1)] += 1.0;
    }
  }
  for (size_t b = 0; b < numBands; b++) {
    // A band the tile never lays down has no pixels to leak from or to,
    // and keeps the identity row it started with.
    if (cellCount[b] == 0) continue;
    for (size_t j = 0; j < numBands; j++) {
      const double mean{neighbors[b * numBands + j] / double(cellCount[b])};
      mixing[b * numBands + j] =
          (b == j ? 1.0 - 4.0 * double(settings.crosstalk[b]) : 0.0) +
          mean * double(settings.crosstalk[j]);
    }
  }
  return mixing;
}

std::vector<double> tileMixingInverse(const ResponseSettings &settings) {
  const size_t numBands{settings.bands.size()};
  std::vector<double> a{tileMixing(settings)};
  std::vector<double> inverse(numBands * numBands, 0.0);
  for (size_t i = 0; i < numBands; i++) inverse[i * numBands + i] = 1.0;
  // Gauss-Jordan with partial pivoting. The matrix is the band count
  // square, which is three for a color camera and never large.
  for (size_t col = 0; col < numBands; col++) {
    size_t pivot{col};
    for (size_t row = col + 1; row < numBands; row++)
      if (std::abs(a[row * numBands + col]) >
          std::abs(a[pivot * numBands + col]))
        pivot = row;
    if (!(std::abs(a[pivot * numBands + col]) > 0)) return {};
    if (pivot != col) {
      for (size_t k = 0; k < numBands; k++) {
        std::swap(a[col * numBands + k], a[pivot * numBands + k]);
        std::swap(inverse[col * numBands + k], inverse[pivot * numBands + k]);
      }
    }
    const double scale{1.0 / a[col * numBands + col]};
    for (size_t k = 0; k < numBands; k++) {
      a[col * numBands + k] *= scale;
      inverse[col * numBands + k] *= scale;
    }
    for (size_t row = 0; row < numBands; row++) {
      if (row == col) continue;
      const double factor{a[row * numBands + col]};
      if (factor == 0) continue;
      for (size_t k = 0; k < numBands; k++) {
        a[row * numBands + k] -= factor * a[col * numBands + k];
        inverse[row * numBands + k] -= factor * inverse[col * numBands + k];
      }
    }
  }
  return inverse;
}

std::vector<ResponseBand> crosstalkFreeBands(const ResponseSettings &settings) {
  if (!settings.hasCFA() || !settings.hasCrosstalk()) return settings.bands;
  const std::vector<double> demix{tileMixingInverse(settings)};
  if (demix.empty()) return settings.bands;
  const size_t numBands{settings.bands.size()};
  // A linear combination of piecewise-linear curves is piecewise linear
  // on the union of their knots, so the values there carry it exactly.
  std::vector<float> knots{};
  for (const auto &band : settings.bands)
    knots.insert(knots.end(), band.wavelengths.begin(), band.wavelengths.end());
  std::sort(knots.begin(), knots.end());
  knots.erase(std::unique(knots.begin(), knots.end()), knots.end());
  std::vector<ResponseBand> bands(numBands);
  for (size_t b = 0; b < numBands; b++) {
    ResponseBand &band{bands[b]};
    band.name = settings.bands[b].name;
    band.wavelengths = knots;
    band.values.reserve(knots.size());
    for (const float knot : knots) {
      double value{};
      for (size_t j = 0; j < numBands; j++)
        value += demix[b * numBands + j] * settings.bands[j].at(double(knot));
      band.values.push_back(float(value));
    }
  }
  return bands;
}

double crosstalkFloor(const ResponseSettings &settings) {
  float peak{0.0f};
  for (const auto &band : settings.bands)
    for (const auto value : band.values) peak = std::max(peak, value);
  if (!(peak > 0)) return 0.0;
  double floor{0.0};
  for (const auto &band : crosstalkFreeBands(settings))
    for (const auto value : band.values)
      floor = std::min(floor, double(value) / double(peak));
  return floor;
}

double ResponseSettings::qeScale() const noexcept {
  if (kind == ResponseKind::QE) return 1.0;
  float peak{0.0f};
  for (const auto &band : bands)
    for (const auto value : band.values) peak = std::max(peak, value);
  return peak > 0 ? double(peakQE.value_or(DEFAULT_PEAK_QE)) / double(peak)
                  : 0.0;
}

SensorDocument parseSensor(LayoutDiagnostics &diags,
                           const LayoutSource &source) {
  SensorDocument document{};
  document.source = &source;
  Parser(diags, source, document).parse();
  // A camera named this file for its sensor, so a file with none is an
  // error rather than an empty sensor; the caret sits at the start,
  // there being nothing else to point at.
  if (!document.sensorLoc && !diags.hasErrors())
    diags.error(LayoutLocation{&source, 0, 1},
                "a sensor file holds one 'sensor' block, and this one has "
                "none");
  return document;
}

SensorDocument readSensor(LayoutDiagnostics &diags,
                          const std::string &fileName) {
  return readDocument(diags, fileName, parseSensor);
}

std::string resolveSensorFileName(const std::string &stated,
                                  const std::string &cameraFileName) {
  // The sidecar this format replaced gets a note saying where its
  // meaning went.
  if (std::filesystem::path(stated).extension() == RESPONSE_EXTENSION)
    throw smdl::Error(smdl::concat(
        "The camera file's 'sensor' ", smdl::QuotedPath(stated),
        " names a '.response' file, a format that no longer exists: the "
        "response is now the 'response' block of a '.sensor' file, which "
        "holds the sensor's 'pixels' and 'pitch' beside it (see "
        "etc/sensors)"));
  return resolveSiblingFile(stated, cameraFileName, "sensor");
}
