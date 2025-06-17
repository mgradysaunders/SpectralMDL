#pragma once

#include <unordered_map>

#include "smdl/common.h"

namespace smdl {

/// \addtogroup Support
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

  void clear() noexcept;

public:
  /// The version string.
  std::string version{};

  /// The properties.
  std::unordered_map<std::string, std::string> properties{};

  class Tilt final {
  public:
    int lampToLuminaireGeometry{};

    /// The angles in degrees.
    std::vector<float> anglesDegrees{};

    /// The multiplying factors.
    std::vector<float> multiplyingFactors{};
  };

  /// The tilt.
  std::optional<Tilt> tilt{};

  /// The number of lamps.
  int numLamps{};

  /// The lumens per lamp.
  int lumensPerLamp{};

  /// The photometric type.
  ///
  /// - `photometricType==1`: Type C
  /// - `photometricType==2`: Type B
  /// - `photometricType==3`: Type A
  ///
  int photometricType{};

  /// The width in meters.
  float widthMeters{};

  /// The length in meters.
  float lengthMeters{};

  /// The height in meters.
  float heightMeters{};

  /// The ballast factor.
  float ballastFactor{};

  /// The input watts.
  float inputWatts{};

  /// The vertical angles in degrees.
  std::vector<float> vertAnglesDegrees{};

  /// The horizontal angles in degrees.
  std::vector<float> horzAnglesDegrees{};

  /// The candela values.
  std::vector<float> candelaValues{};
};

/// \}

} // namespace smdl
