#pragma once

#include <unordered_map>

#include "smdl/common.h"

namespace smdl {

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
      static_assert(sizeof(float3) == 16);
      return sizeof(float3);
    default:
      return 0;
    }
  }

  BSDFMeasurement() = default;

  BSDFMeasurement(const BSDFMeasurement &) = delete;

  ~BSDFMeasurement() { clear(); }

  [[nodiscard]] static std::unique_ptr<BSDFMeasurement>
  load_from_memory(const std::string &file);

  [[nodiscard]] static std::unique_ptr<BSDFMeasurement>
  load_from_file(const std::string &fileName);

  void clear() noexcept;

public:
  Kind kind{KIND_REFLECTION};

  Type type{TYPE_FLOAT};

  size_t numTheta{};

  size_t numPhi{};

  void *buffer{};

  std::unordered_map<std::string, std::string> metaData{};
};

} // namespace smdl
