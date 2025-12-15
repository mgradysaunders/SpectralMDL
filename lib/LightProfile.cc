#include "smdl/LightProfile.h"

#include "llvm/ADT/StringRef.h"

namespace smdl {

std::optional<Error>
LightProfile::loadFromFileMemory(std::string file) noexcept {
  clear();
  std::replace(file.begin(), file.end(), ',', ' ');
  auto error{catchAndReturnError([&] {
    auto text{llvm::StringRef(file).trim()};
    if (!text.starts_with("IESNA"))
      throw Error("not an IES file");
    // Parse version
    {
      auto [line, remainder] = text.split('\n');
      version = std::string(line.trim());
      // IESNA:LM-63-1986
      // IESNA:LM-63-1991
      // IESNA:LM-63-1995
      // IESNA91
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
        throw Error(concat("unsupported tilt ", Quoted(tiltKind)));
      text = remainder;
      if (tiltKind == "INCLUDE") {
        tilt = Tilt();
        size_t numAngles{};
        parseNumberOrThrow("lamp to luminaire geometry",
                           tilt->lampToLuminaireGeometry);
        parseNumberOrThrow("num tilt angles", numAngles);
        tilt->angles.resize(numAngles);
        tilt->multiplyingFactors.resize(numAngles);
        for (auto &angle : tilt->angles)
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
    parseNumberOrThrow("photometric type", photometryType);
    parseNumberOrThrow("units type", unitsType);
    parseNumberOrThrow("width", width);
    parseNumberOrThrow("length", length);
    parseNumberOrThrow("height", height);
    if (photometryType != 1 && photometryType != 2 && photometryType != 3)
      throw Error(concat("unknown photometric type ", photometryType));
    if (unitsType != 1 && unitsType != 2)
      throw Error(concat("unknown units type ", unitsType));

    // Unit conversion of feet to meters
    float unitConversion{unitsType == 1 ? 0.3048f : 1.0f};
    width *= unitConversion;
    length *= unitConversion;
    height *= unitConversion;

    float ballastFactor{};
    float futureUse{};
    parseNumberOrThrow("ballast factor", ballastFactor);
    parseNumberOrThrow("ballast lamp photometric factor", futureUse);
    parseNumberOrThrow("input watts", inputWatts);
    inputWatts *= multiplier * ballastFactor;

    vertAngles.resize(numVertAngles);
    horzAngles.resize(numHorzAngles);
    for (auto &vertAngle : vertAngles) {
      parseNumberOrThrow("vertical angle", vertAngle);
    }
    for (auto &horzAngle : horzAngles) {
      parseNumberOrThrow("horizontal angle", horzAngle);
    }
    intensityValues.resize(numVertAngles * numHorzAngles);
    for (auto &intensityValue : intensityValues) {
      parseNumberOrThrow("candela value", intensityValue);
      intensityValue *= multiplier * ballastFactor;
      intensityValue /= 683.0f; // Convert to W/sr?
    }
  })};
  if (error) {
    clear();
  }
  return error;
}

std::optional<Error>
LightProfile::loadFromFile(const std::string &fileName) noexcept {
  auto file{std::string()};
  if (auto error{catchAndReturnError([&] { file = readOrThrow(fileName); })})
    return error;
  if (auto error{loadFromFileMemory(std::move(file))})
    return Error(
        concat("cannot load ", QuotedPath(fileName), ": ", error->message));
  return std::nullopt;
}

void LightProfile::clear() noexcept {
  version.clear();
  properties.clear();
  tilt.reset();
  numLamps = 0;
  lumensPerLamp = 0;
  photometryType = 0;
  width = 0;
  length = 0;
  height = 0;
  inputWatts = 0;
  vertAngles.clear();
  horzAngles.clear();
  intensityValues.clear();
}

float LightProfile::power() const noexcept {
  // TODO Actually calculate this instead of relying on
  //      measured electric power consumption
  return inputWatts;
}

struct LerpLookup final {
  int index0{};
  int index1{};
  float fraction{};
};

[[nodiscard]] static LerpLookup lerpLookup(const std::vector<float> &values,
                                           float value) {
  auto itr1{std::lower_bound(values.begin(), values.end(), value)};
  if (itr1 == values.end()) {
    return {int(values.size()) - 1, int(values.size()) - 1, 0.0f};
  }
  auto itr0{itr1};
  if (itr0 != values.begin()) {
    --itr0;
  }
  float value0{*itr0};
  float value1{*itr1};
  float fraction{(value - value0) / (value1 - value0)};
  if (!std::isfinite(fraction))
    fraction = 0;
  return {int(itr0 - values.begin()), //
          int(itr1 - values.begin()), //
          fraction};
}

[[nodiscard]] static float lerp(float fraction, float value0, float value1) {
  return (1 - fraction) * value0 + fraction * value1;
}

float LightProfile::interpolate(float3 wo) const noexcept {
  wo = normalize(wo);
  if (!std::isfinite(wo.x) || !std::isfinite(wo.y) || !std::isfinite(wo.z) ||
      vertAngles.empty()) {
    return 0;
  }
  auto atan2Positive{[](float y, float x) {
    // By default, STL atan2 returns a value in radians between
    // negative and positive pi. The implementation here scales
    // to a value in degrees between negative and positive 180,
    // then biases negative values so that the result is between
    // zero and 360.
    float theta{180.0f / 3.14159265359f * std::atan2(y, x)};
    if (theta < 0.0f)
      theta += 180.0f;
    theta = std::fmax(theta, 0.0f);
    theta = std::fmin(theta, 360.0f);
    return theta;
  }};
  if (photometryType == 1) {
    // > Type C photometry is normally used for architectural and
    // > roadway luminaires. The polar axis of the photometric web
    // > coincides with the vertical axis of the luminaire, and the
    // > 0-180 degree photometric plane coincides with the luminaire's
    // > major axis (length).
    // >
    // > [...]
    // >
    // > For Type C photometry, the first vertical angle will be either
    // > 0 or 90 degrees, and the last vertical angle will be either 90
    // > or 180 degrees.
    // >
    // > [...]
    // >
    // > For Type C photometry, the first value is (almost) always
    // > 0 degrees, and the last value is one of the following:
    // >   0  There is only one horizontal angle, implying that the
    // >      luminaire is laterally symmetric in all photometric
    // >      planes.
    // >  90  The luminaire is assumed to be symmetric in each quadrant
    // > 180  The luminaire is assumed to be bilaterally symmetric about
    // >      the 0-180 degree photometric plane.
    // > 360  The luminaire is assumed to exhibit no lateral symmetry.
    // >      (NOTE: This is an error in the draft IES LM-63-1995 standard,
    // >      because the 360-degree plane is coincident with the 0-degree
    // >      plane. It should read "greater than 180 degrees and less than
    // >      360 degrees).
    // >
    // > (A luminaire that is bilaterally symmetric about the 90-270
    // > degree photometric plane will have a first value of 90 degrees
    // > and a last value of 270 degrees)
    float vertAngle{atan2Positive(std::hypot(wo.x, wo.y), wo.z)};
    if (!(vertAngles.front() <= vertAngle && vertAngle <= vertAngles.back())) {
      return 0.0f;
    }
    if (horzAngles.size() <= 1) {
      auto lookup{lerpLookup(vertAngles, vertAngle)};
      return lerp(lookup.fraction, intensityValues[lookup.index0],
                  intensityValues[lookup.index1]);
    } else {
      float horzAngle{[&]() {
        const int minHorzAngle{int(std::rint(horzAngles.front()))};
        const int maxHorzAngle{int(std::rint(horzAngles.back()))};
        if (minHorzAngle == 0 && maxHorzAngle == 90) {
          return atan2Positive(std::abs(wo.y), std::abs(wo.x));
        } else if (minHorzAngle == 0 && maxHorzAngle == 180) {
          return atan2Positive(std::abs(wo.y), wo.x);
        } else if (minHorzAngle == 90 && maxHorzAngle == 270) {
          return atan2Positive(std::abs(wo.x), -wo.y);
        } else if (minHorzAngle == 0 && maxHorzAngle > 180) {
          return atan2Positive(wo.y, wo.x);
        } else {
          SMDL_SANITY_CHECK(
              false, "Unexpected horizontal angles for Type C photometry!");
          return 0.0f;
        }
      }()};
      auto vertLookup{lerpLookup(vertAngles, vertAngle)};
      auto horzLookup{lerpLookup(horzAngles, horzAngle)};
      auto intensityRow0{&intensityValues[0] +
                         vertAngles.size() * horzLookup.index0};
      auto intensityRow1{&intensityValues[0] +
                         vertAngles.size() * horzLookup.index1};
      return lerp(horzLookup.fraction,
                  lerp(vertLookup.fraction, intensityRow0[vertLookup.index0],
                       intensityRow0[vertLookup.index1]),
                  lerp(vertLookup.fraction, intensityRow1[vertLookup.index0],
                       intensityRow1[vertLookup.index1]));
    }
  } else {
    // > Type B photometry is normally used for adjustable outdoor area
    // > and sports lighting luminaires. The polar axis of the luminaire
    // > coincides with the minor axis (width) of the luminaire, and the
    // > 0-180 degree photometric plane coincides with the luminaire's
    // > vertical axis.
    // >
    // > Type A photometry is normally used for automotive headlights and
    // > signal lights. The polar axis of the luminaire coincides with the
    // > major axis (length) of the luminaire, and the 0-180 degree
    // > photometric plane coincides with the luminaire's vertical axis.
    // >
    // > [...]
    // >
    // > For Type A or B photometry, the first vertical angle will be
    // > either -90 or 0 degrees, and the last vertical angle will be 90
    // > degrees.
    // >
    // > [...]
    // >
    // > For Type A or B photometry where the luminaire is laterally
    // > symmetric about a vertical reference plane, the first horizontal
    // > angle will be 0 degrees, and the last horizontal angle will be
    // > 90 degrees.
    // >
    // > For Type A or B photometry where the luminaire is not laterally
    // > symmetric about a vertical reference plane, the first horizontal
    // > angle will be -90 degrees, and the last horizontal angle will be
    // > 90 degrees.

    // TODO
  }
  return 0;
}

extern "C" {

SMDL_EXPORT float smdl_light_profile_interpolate(const void *profile_ptr,
                                                 const float3 &wo) {
  return profile_ptr
             ? static_cast<const LightProfile *>(profile_ptr)->interpolate(wo)
             : 0.0f;
}

} // extern "C"

} // namespace smdl
