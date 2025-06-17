#include "smdl/LightProfile.h"

#include "filesystem.h"

#include "llvm/ADT/StringRef.h"

namespace smdl {

std::optional<Error>
LightProfile::load_from_file_memory(std::string file) noexcept {
  clear();
  std::replace(file.begin(), file.end(), ',', ' ');
  auto error{catch_and_return_error([&] {
    auto text{llvm::StringRef(file)};
    if (!text.starts_with("IESNA"))
      throw Error("not an IES file");

    // Parse version
    {
      auto [line, remainder] = text.split('\n');
      version = std::string(line.trim());
      // IESNA:LM-63-1986
      // IESNA:LM-63-1991
      // IESNA:LM-63-1995
      text = remainder;
    }

    // Parse properties
    while (true) {
      auto [line, remainder] = text.ltrim().split('\n');
      auto brackL{line.find('[')};
      auto brackR{line.find(']')};
      if (!(brackL < brackR && brackR < line.size()))
        break;
      auto key{line.substr(brackL + 1, brackR - brackL - 1).trim()};
      auto val{line.substr(brackR + 1).trim()};
      properties[std::string(key)] = std::string(val);
      text = remainder;
    }

    auto parseNumberOrThrow{[&](const char *what, auto &value) {
      text = text.ltrim();
      if constexpr (std::is_floating_point_v<std::decay_t<decltype(value)>>) {
        auto result{double(0)};
        auto num{text.take_until(is_space)};
        if (!num.getAsDouble(result, /*AllowInexact=*/true)) {
          value = result;
          text = text.drop_front(num.size());
        } else {
          throw Error(concat("expected float: ", what));
        }
      } else {
        if (text.consumeInteger(/*Radix=*/0, value)) {
          throw Error(concat("expected int: ", what));
        }
      }
    }};

    // Parse tilt
    {
      auto [line, remainder] = text.ltrim().split('\n');
      if (!line.consume_front("TILT="))
        throw Error("expected 'TILT='");
      auto tiltKind{std::string(line.trim())};
      if (tiltKind != "NONE" && tiltKind != "INCLUDE")
        throw Error(concat("unsupported tilt ", quoted(tiltKind)));
      text = remainder;
      if (tiltKind == "INCLUDE") {
        tilt = Tilt();
        size_t numAngles{};
        parseNumberOrThrow("lamp to luminaire geometry",
                           tilt->lampToLuminaireGeometry);
        parseNumberOrThrow("num tilt angles", numAngles);
        tilt->anglesDegrees.resize(numAngles);
        tilt->multiplyingFactors.resize(numAngles);
        for (auto &angle : tilt->anglesDegrees)
          parseNumberOrThrow("tilt angle", angle);
        for (auto &multiplyingFactor : tilt->multiplyingFactors)
          parseNumberOrThrow("tilt multiplying factor", multiplyingFactor);
      }
    }

    float multiplier{};
    size_t numVertAngles{};
    size_t numHorzAngles{};
    int unitsType{};
    parseNumberOrThrow("num lamps", numLamps);
    parseNumberOrThrow("lumens per lamp", lumensPerLamp);
    parseNumberOrThrow("multiplier", multiplier);
    parseNumberOrThrow("num vertical angles", numVertAngles);
    parseNumberOrThrow("num horizontal angles", numHorzAngles);
    parseNumberOrThrow("photometric type", photometricType);
    parseNumberOrThrow("units type", unitsType);
    parseNumberOrThrow("width", widthMeters);
    parseNumberOrThrow("length", lengthMeters);
    parseNumberOrThrow("height", heightMeters);
    if (photometricType != 1 && photometricType != 2 && photometricType != 3)
      throw Error(concat("unknown photometric type ", photometricType));
    if (unitsType != 1 && unitsType != 2)
      throw Error(concat("unknown units type ", unitsType));

    // Unit conversion of feet to meters
    float unitConversion{unitsType == 1 ? 0.3048f : 1.0f};
    widthMeters *= unitConversion;
    lengthMeters *= unitConversion;
    heightMeters *= unitConversion;

    float futureUse{};
    parseNumberOrThrow("ballast factor", ballastFactor);
    parseNumberOrThrow("ballast lamp photometric factor", futureUse);
    parseNumberOrThrow("input watts", inputWatts);

    vertAnglesDegrees.resize(numVertAngles);
    horzAnglesDegrees.resize(numHorzAngles);
    for (auto &vertAngle : vertAnglesDegrees) {
      parseNumberOrThrow("vertical angle", vertAngle);
    }
    for (auto &horzAngle : horzAnglesDegrees) {
      parseNumberOrThrow("horizontal angle", horzAngle);
    }
    candelaValues.resize(numVertAngles * numHorzAngles);
    for (auto &candelaValue : candelaValues) {
      parseNumberOrThrow("candela value", candelaValue);
      candelaValue *= multiplier;
    }
  })};
  if (error) {
    clear();
  }
  return error;
}

std::optional<Error>
LightProfile::load_from_file(const std::string &fileName) noexcept {
  auto file{std::string()};
  if (auto error{catch_and_return_error([&] { file = fs_read(fileName); })})
    return error;
  if (auto error{load_from_file_memory(std::move(file))})
    return Error(concat("cannot load ", quoted(fs_abbreviate(fileName)), ": ",
                        error->message));
  return std::nullopt;
}

void LightProfile::clear() noexcept {
  version.clear();
  properties.clear();
  tilt.reset();
  numLamps = 0;
  lumensPerLamp = 0;
  photometricType = 0;
  widthMeters = 0;
  lengthMeters = 0;
  heightMeters = 0;
  ballastFactor = 0;
  inputWatts = 0;
  vertAnglesDegrees.clear();
  horzAnglesDegrees.clear();
  candelaValues.clear();
}

} // namespace smdl
