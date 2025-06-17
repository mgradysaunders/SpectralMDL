#pragma once

#include <unordered_map>

#include "smdl/common.h"

namespace smdl {

/// \addtogroup Support
/// \{

class SMDL_EXPORT BSDFMeasurement final {
public:
  enum Kind { KIND_REFLECTION, KIND_TRANSMISSION };

  enum Type { TYPE_FLOAT, TYPE_FLOAT3 };

  [[nodiscard]] static constexpr size_t size_of(Type type) noexcept {
    switch (type) {
    case TYPE_FLOAT:
      static_assert(sizeof(float) == 4);
      return sizeof(float);
    case TYPE_FLOAT3:
      static_assert(sizeof(float3) == 16); // Aligned to 16
      return sizeof(float3);
    default:
      return 0;
    }
  }

  BSDFMeasurement() = default;

  BSDFMeasurement(const BSDFMeasurement &) = delete;

  ~BSDFMeasurement() { clear(); }

  /// Load from file memory.
  [[nodiscard]]
  std::optional<Error> load_from_file_memory(const std::string &file) noexcept;

  /// Load from file.
  [[nodiscard]]
  std::optional<Error> load_from_file(const std::string &fileName) noexcept;

  void clear() noexcept;

public:
  /// The kind.
  Kind kind{KIND_REFLECTION};

  /// The type.
  Type type{TYPE_FLOAT};

  /// The number of samples in zenith.
  size_t numTheta{};

  /// The number of samples in azimuth.
  size_t numPhi{};

  /// The buffer of `numTheta * numTheta * numPhi` samples either of type
  /// `float` or `float3`.
  void *buffer{};

  /// The meta-data.
  std::unordered_map<std::string, std::string> metaData{};
};

/// \}

} // namespace smdl
