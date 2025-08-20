/// \file
#pragma once

#include <unordered_map>

#include "smdl/common.h"

namespace smdl {

/// \addtogroup Main
/// \{

/// An IES light profile.
class SMDL_EXPORT LightProfile final {
public:
  /// Load from file memory.
  [[nodiscard]]
  std::optional<Error> load_from_file_memory(std::string file) noexcept;

  /// Load from file.
  [[nodiscard]]
  std::optional<Error> load_from_file(const std::string &fileName) noexcept;

  /// Clear.
  void clear() noexcept;

  /// Is valid?
  [[nodiscard]] bool is_valid() const noexcept {
    return !intensityValues.empty();
  }

  /// Calculate the radiometric max intensity.
  [[nodiscard]] float max_intensity() const noexcept {
    float result{};
    for (float intensityValue : intensityValues)
      result = std::max(result, intensityValue);
    return result;
  }

  /// Calculate the radiometric power.
  [[nodiscard]] float power() const noexcept;

  /// Interpolate.
  [[nodiscard]] float interpolate(float3 wo) const noexcept;


public:
  /// The version string.
  std::string version{};

  /// The properties.
  std::unordered_map<std::string, std::string> properties{};

  class Tilt final {
  public:
    int lampToLuminaireGeometry{};

    /// The angles in degrees.
    std::vector<float> angles{};

    /// The multiplying factors.
    std::vector<float> multiplyingFactors{};
  };

  /// The tilt.
  std::optional<Tilt> tilt{};

  /// The number of lamps.
  int numLamps{};

  /// The lumens per lamp.
  float lumensPerLamp{};

  /// The photometry type.
  ///
  /// - `photometryType==1`: Type C
  /// - `photometryType==2`: Type B
  /// - `photometryType==3`: Type A
  ///
  int photometryType{};

  /// The length in meters of the luminous opening.
  ///
  /// \note
  /// This is measured along the _major axis_, which we
  /// understand to be the X axis.
  ///
  float length{};

  /// The width in meters of the luminous opening.
  ///
  /// \note
  /// This is measured along the _minor axis_, which we
  /// understand to be the Y axis.
  ///
  float width{};

  /// The height in meters of the luminous opening.
  ///
  /// \note
  /// This is measured along the _vertical axis_, which we
  /// understand to be the Z axis.
  ///
  float height{};

  /// The input watts.
  float inputWatts{};

  /// The vertical angles in degrees.
  std::vector<float> vertAngles{};

  /// The horizontal angles in degrees.
  std::vector<float> horzAngles{};

  /// The intensity values in Watts per steradian.
  ///
  /// \note
  /// The implementation pre-multiplies all candela values by
  /// the `multiplier` and `ballastFactor` in the IES file.
  ///
  std::vector<float> intensityValues{};
};

/// \}

} // namespace smdl
