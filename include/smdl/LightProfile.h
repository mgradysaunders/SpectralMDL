#pragma once

#include <unordered_map>

#include "smdl/common.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// An IES light profile.
class SMDL_EXPORT LightProfile final {
public:
  [[nodiscard]]
  std::optional<Error> load_from_file_memory(std::string file) noexcept;

  [[nodiscard]]
  std::optional<Error> load_from_file(const std::string &fileName) noexcept;

  void clear() noexcept;

public:
  std::string version{};

  std::unordered_map<std::string, std::string> properties{};

  class Tilt final {
  public:
    int lampToLuminaireGeometry{};

    std::vector<float> anglesDegrees{};

    std::vector<float> multiplyingFactors{};
  };

  std::optional<Tilt> tilt{};

  int numLamps{};

  int lumensPerLamp{};

  // 1 = Type C
  // 2 = Type B
  // 3 = Type A
  int photometricType{};

  float widthMeters{};

  float lengthMeters{};

  float heightMeters{};

  float ballastFactor{};

  float inputWatts{};

  std::vector<float> vertAnglesDegrees{};

  std::vector<float> horzAnglesDegrees{};

  std::vector<float> candelaValues{};
};

/// \}

} // namespace smdl
